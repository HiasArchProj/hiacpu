package org.chipsalliance.hia

import chisel3._
import chisel3.util._
import chisel3.experimental.hierarchy.{instantiable, public, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import javax.swing.plaf.nimbus.NimbusLookAndFeel
import chisel3.experimental.BundleLiterals._


class FetchExecutePipelineReg(xlen: Int) extends Bundle {
  val inst = chiselTypeOf(Instructions.NOP)
  val pc = UInt(xlen.W)
}

class ExecuteWritebackPipelineReg(xlen: Int) extends  Bundle {
  val inst = chiselTypeOf(Instructions.NOP)
  val pc = UInt(xlen.W)
  val alu = UInt(xlen.W)
  val csr_in = UInt(xlen.W)
}

object DatapathParameter {
  implicit def rwP: upickle.default.ReadWriter[DatapathParameter] =
    upickle.default.macroRW[DatapathParameter]
}

case class DatapathParameter(xlen: Int, ctrl: ControlParameter) extends SerializableModuleParameter {
  val aluParameter = ALUParameter(xlen)
  val brCondParameter = BrCondParameter(xlen, ctrl)
  val csrParameter = CSRParameter(xlen, ctrl)

  val PC_START = 0x200
}

class DatapathInterface(parameter: DatapathParameter) extends Bundle {
  val icache = Flipped(new ICacheIO(parameter.xlen))
  val dcache = Flipped(new DCacheIO(parameter.xlen))
  val ctrl = Flipped(new ControlInterface(parameter.ctrl))
}

@instantiable
class Datapath(val parameter: DatapathParameter)
    extends FixedIORawModule(new DatapathInterface(parameter))
    with SerializableModule[DatapathParameter]
    with Public {
  // TODO CSRmodule
  val xlen = parameter.xlen
  val PC_START = parameter.PC_START
  val PC_EPC = parameter.ctrl.PC_EPC
  val PC_ALU = parameter.ctrl.PC_ALU
  val PC_0 = parameter.ctrl.PC_0
  val WB_ALU = parameter.ctrl.WB_ALU
  val WB_MEM = parameter.ctrl.WB_MEM
  val WB_PC4 = parameter.ctrl.WB_PC4
  val WB_CSR = parameter.ctrl.WB_CSR
  val ST_SW = parameter.ctrl.ST_SW
  val ST_SH = parameter.ctrl.ST_SH
  val ST_SB = parameter.ctrl.ST_SB
  val LD_LB = parameter.ctrl.LD_LB
  val LD_LH = parameter.ctrl.LD_LH
  val LD_LHU = parameter.ctrl.LD_LHU
  val LD_LBU = parameter.ctrl.LD_LBU
  val A_RS1 = parameter.ctrl.A_RS1
  val B_RS2 = parameter.ctrl.B_RS2
  val IMM_Z = parameter.ctrl.IMM_Z


  val regFile = Module(new RegFile(parameter.xlen))
  val csr = Instantiate(new CSR(parameter.csrParameter))
  val alu = Instantiate(new ALU(parameter.aluParameter))
  val brCond = Instantiate(new BrCond(parameter.brCondParameter))

  /** Pipeline State Registers * */

  /** *** Fetch / Execute Registers ****
    */
  val fe_reg = RegInit(
    (new FetchExecutePipelineReg(xlen)).Lit(
      _.inst -> Instructions.NOP,
      _.pc -> 0.U
    )
  )

  /** *** Execute / Write Back Registers ****
    */
  val ew_reg = RegInit(
    (new ExecuteWritebackPipelineReg(xlen)).Lit(
      _.inst -> Instructions.NOP,
      _.pc -> 0.U,
      _.alu -> 0.U,
      _.csr_in -> 0.U
    )
  )

  /** **** Control signals ****
    */
  val st_type = Reg(io.ctrl.st_type.cloneType)
  val ld_type = Reg(io.ctrl.ld_type.cloneType)
  val wb_sel = Reg(io.ctrl.wb_sel.cloneType)
  val wb_en = Reg(Bool())
  val csr_cmd = Reg(io.ctrl.csr_cmd.cloneType)
  val illegal = Reg(Bool())
  val pc_check = Reg(Bool())

  /** **** Fetch ****
    */
  val started = RegNext(Bool())
  val stall = !io.icache.valid || !io.dcache.valid
  val pc = RegInit(PC_START.U(xlen.W) - 4.U(xlen.W))
  // Next Program Counter
  val next_pc = MuxCase(
    pc + 4.U,
    IndexedSeq(
      stall -> pc,
      csr.io.expt -> csr.io.evec,
      (io.ctrl.pc_sel === PC_EPC) -> csr.io.epc,
      ((io.ctrl.pc_sel === PC_ALU) || (brCond.io.taken)) -> (alu.io.sum >> 1.U << 1.U),
      (io.ctrl.pc_sel === PC_0) -> pc
    )
  )
  val inst =
    Mux(started || io.ctrl.inst_kill || brCond.io.taken || csr.io.expt, Instructions.NOP, io.icache.data)
  pc := next_pc
  io.icache.addr := next_pc
  io.icache.data := 0.U
  io.icache.mask := 0.U
  io.icache.valid := !stall

  // Pipelining
  when(!stall) {
    fe_reg.pc := pc
    fe_reg.inst := inst
  }

  /** **** Execute ****
    */
  io.ctrl.inst := fe_reg.inst

  // regFile read
  val rd_addr = fe_reg.inst(11, 7)
  val rs1_addr = fe_reg.inst(19, 15)
  val rs2_addr = fe_reg.inst(24, 20)
  regFile.io.raddr1 := rs1_addr
  regFile.io.raddr2 := rs2_addr

  // gen immdeates
  // immGen.io.inst := fe_reg.inst
  // immGen.io.sel := io.ctrl.imm_sel
  val imm_out = ImmGen(io.ctrl.imm_sel, fe_reg.inst)

  // bypass
  val wb_rd_addr = ew_reg.inst(11, 7)
  val rs1hazard = wb_en && rs1_addr.orR && (rs1_addr === wb_rd_addr)
  val rs2hazard = wb_en && rs2_addr.orR && (rs2_addr === wb_rd_addr)
  val rs1 = Mux(wb_sel === WB_ALU && rs1hazard, ew_reg.alu, regFile.io.rdata1)
  val rs2 = Mux(wb_sel === WB_ALU && rs2hazard, ew_reg.alu, regFile.io.rdata2)

  // ALU operations
  alu.io.A := Mux(io.ctrl.A_sel === A_RS1, rs1, fe_reg.pc)
  alu.io.B := Mux(io.ctrl.B_sel === B_RS2, rs2, imm_out)
  alu.io.alu_op := io.ctrl.alu_op

  // Branch condition calc
  brCond.io.rs1 := rs1
  brCond.io.rs2 := rs2
  brCond.io.br_type := io.ctrl.br_type

  // D$ access
  val daddr = Mux(stall, ew_reg.alu, alu.io.sum) >> 2.U << 2.U
  val woffset = (alu.io.sum(1) << 4.U).asUInt | (alu.io.sum(0) << 3.U).asUInt
  io.dcache.valid := !stall && (io.ctrl.st_type.orR || io.ctrl.ld_type.orR)
  io.dcache.addr := daddr
  io.dcache.data := rs2 << woffset
  io.dcache.mask := MuxLookup(Mux(stall, st_type, io.ctrl.st_type), "b0000".U)(
    Seq(ST_SW -> "b1111".U, ST_SH -> ("b11".U << alu.io.sum(1, 0)), ST_SB -> ("b1".U << alu.io.sum(1, 0)))
  )

  // Pipelining
  // when(io.reset.asBool || !stall && csr.io.expt) {
  when(!stall && csr.io.expt) {
    st_type := 0.U
    ld_type := 0.U
    wb_en := false.B
    csr_cmd := 0.U
    illegal := false.B
    pc_check := false.B
  }.elsewhen(!stall && !csr.io.expt) {
    ew_reg.pc := fe_reg.pc
    ew_reg.inst := fe_reg.inst
    ew_reg.alu := alu.io.out
    ew_reg.csr_in := Mux(io.ctrl.imm_sel === IMM_Z, imm_out, rs1)
    st_type := io.ctrl.st_type
    ld_type := io.ctrl.ld_type
    wb_sel := io.ctrl.wb_sel
    wb_en := io.ctrl.wb_en
    csr_cmd := io.ctrl.csr_cmd
    illegal := io.ctrl.illegal
    pc_check := io.ctrl.pc_sel === PC_ALU
  }

  // Load
  val loffset = (ew_reg.alu(1) << 4.U).asUInt | (ew_reg.alu(0) << 3.U).asUInt
  val lshift = io.dcache.data >> loffset
  val load = MuxLookup(ld_type, io.dcache.data.zext)(
    Seq(
      LD_LH -> lshift(15, 0).asSInt,
      LD_LB -> lshift(7, 0).asSInt,
      LD_LHU -> lshift(15, 0).zext,
      LD_LBU -> lshift(7, 0).zext
    )
  )

  // CSR access
  csr.io.stall := stall
  csr.io.in := ew_reg.csr_in
  csr.io.cmd := csr_cmd
  csr.io.inst := ew_reg.inst
  csr.io.pc := ew_reg.pc
  csr.io.addr := ew_reg.alu
  csr.io.illegal := illegal
  csr.io.pc_check := pc_check
  csr.io.ld_type := ld_type
  csr.io.st_type := st_type

  // Regfile Write
  val regWrite =
    MuxLookup(wb_sel, ew_reg.alu.zext)(
      Seq(WB_MEM -> load, WB_PC4 -> (ew_reg.pc + 4.U).zext, WB_CSR -> csr.io.out.zext)
    ).asUInt

  regFile.io.wen := wb_en && !stall && !csr.io.expt
  regFile.io.waddr := wb_rd_addr
  regFile.io.wdata := regWrite  

}

class RegFileIO(xlen: Int) extends Bundle {
  val raddr1 = Input(UInt(5.W))
  val raddr2 = Input(UInt(5.W))
  val rdata1 = Output(UInt(xlen.W))
  val rdata2 = Output(UInt(xlen.W))
  val wen = Input(Bool())
  val waddr = Input(UInt(5.W))
  val wdata = Input(UInt(xlen.W))
}

class RegFile(xlen: Int) extends Module {
  val io = IO(new RegFileIO(xlen))
  val regs = Mem(32, UInt(xlen.W))
  io.rdata1 := Mux(io.raddr1.orR, regs(io.raddr1), 0.U)
  io.rdata2 := Mux(io.raddr2.orR, regs(io.raddr2), 0.U)
  when(io.wen & io.waddr.orR) {
    regs(io.waddr) := io.wdata
  }
}
