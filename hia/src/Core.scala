package org.chipsalliance.hia

import chisel3._
import chisel3.util._
import chisel3.experimental.hierarchy.{instantiable, public, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.util.experimental.decode.decoder

object FetchStageParameter {
  implicit def rwP: upickle.default.ReadWriter[FetchStageParameter] =
    upickle.default.macroRW[FetchStageParameter]
}

case class FetchStageParameter(xlen: Int, decoderParameter: DecoderParameter) extends SerializableModuleParameter {
  val PC_START = "h_8000_0000".U(xlen.W)
  val PC_ALU = decoderParameter.PC_ALU.U(decoderParameter.PC_SEL_LEN.W)
  val PC_EPC = decoderParameter.PC_EPC.U(decoderParameter.PC_SEL_LEN.W)
}


class FetchStageInterface(parameter: FetchStageParameter) extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())

  val out = Decoupled(new FetchStageMessage(parameter.xlen))
  val icache = Flipped(new ICacheIO(parameter.xlen)) // FIXME redundant interface exists, use least interface
  val bypass = Flipped(new ExecuteStageBypassMessage(parameter.xlen, parameter.decoderParameter.PC_SEL_LEN))
}

@instantiable
class FetchStage(val parameter: FetchStageParameter)
    extends FixedIORawModule(new FetchStageInterface(parameter))
    with SerializableModule[FetchStageParameter]
    with Public 
    with ImplicitClock
    with ImplicitReset {
  override protected def implicitClock: Clock = io.clock
  override protected def implicitReset: Reset = io.reset
  val xlen = parameter.xlen

  val pc = RegInit(parameter.PC_START)
  val branch = (io.bypass.pc_sel === parameter.PC_ALU) || io.bypass.taken
  val epc = (io.bypass.pc_sel === parameter.PC_EPC) 

  val s_idle :: s_after_branch :: Nil = Enum(2)
  val state = RegInit(s_idle)
  state := MuxLookup(state, s_idle)(Seq(
    s_idle -> Mux(branch, s_after_branch, s_idle),
    s_after_branch -> s_idle
  ))

  io.out.valid := (~io.reset.asBool) && (~branch || (state === s_after_branch)) && (~epc)// FIXME maybe wrong

  val next_pc = MuxCase(
    (pc+4.U),
    Seq(
      (branch) -> Cat(io.bypass.alu_out(xlen - 1, 1), 0.U(1.W)),
      (epc) -> io.bypass.epc,
      (~io.out.ready) -> pc
    )
  )
  pc := next_pc

  // connect cache and stage reg
  io.icache.raddr := pc
  io.out.bits.pc := pc
}



object ExecuteStageParameter {
  implicit def rwP: upickle.default.ReadWriter[ExecuteStageParameter] =
    upickle.default.macroRW[ExecuteStageParameter]
}

case class ExecuteStageParameter(xlen: Int, decoderParameter: DecoderParameter) extends SerializableModuleParameter {
  val aluParameter = ALUParameter(xlen, decoderParameter)
  val brCondParameter = BrCondParameter(xlen, decoderParameter)
}


class ExecuteStageInterface(parameter: ExecuteStageParameter) extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())

  val out = Decoupled(new ExecuteStageMessage(parameter.xlen, parameter.decoderParameter))
  val in = Flipped(Decoupled(new FetchStageMessage(parameter.xlen)))

  val inst = Input(UInt(parameter.xlen.W))
  val gpr_read = Flipped(new regReadIO(parameter.xlen))
  val csr = Flipped(new CSRFileIO(parameter.xlen))
  val bypass_out = new ExecuteStageBypassMessage(parameter.xlen, parameter.decoderParameter.PC_SEL_LEN)
  val bypass_in = Flipped(new WriteBackStageBypassMessage(parameter.xlen, parameter.decoderParameter.WB_SEL_LEN))
}

@instantiable
class ExecuteStage(val parameter: ExecuteStageParameter)
    extends FixedIORawModule(new ExecuteStageInterface(parameter))
    with SerializableModule[ExecuteStageParameter]
    with Public 
    with ImplicitClock
    with ImplicitReset {
  override protected def implicitClock: Clock = io.clock
  override protected def implicitReset: Reset = io.reset

  val xlen = parameter.xlen
  val inst = io.inst
  val pc = io.in.bits.pc

  val decoder: Instance[Decoder] = Instantiate(new Decoder(parameter.decoderParameter))
  val decodeOutput = Wire(chiselTypeOf(decoder.io.output))
  decoder.io.instruction := inst
  decodeOutput := decoder.io.output
  val alu: Instance[ALU] = Instantiate(new ALU(parameter.aluParameter))

  io.bypass_out.pc_sel := decodeOutput(parameter.decoderParameter.selPC)
  val imm_out = ImmGen(decodeOutput(parameter.decoderParameter.immType), inst)

  // read reg
  io.gpr_read.req.addr1 := inst(19, 15)
  io.gpr_read.req.addr2 := inst(24, 20)

  // bypass
  val rs1hazard = io.bypass_in.wb_en && io.gpr_read.req.addr1.orR && (io.gpr_read.req.addr1 === io.bypass_in.wb_rd_addr)
  val rs2hazard = io.bypass_in.wb_en && io.gpr_read.req.addr2.orR && (io.gpr_read.req.addr2 === io.bypass_in.wb_rd_addr)
  val rs1 = Mux(io.bypass_in.wb_sel === parameter.decoderParameter.WB_ALU.U && rs1hazard, io.bypass_in.wb_data, io.gpr_read.rsp.data1)
  val rs2 = Mux(io.bypass_in.wb_sel === parameter.decoderParameter.WB_ALU.U && rs2hazard, io.bypass_in.wb_data, io.gpr_read.rsp.data2)

  // csr
  val csr_data = io.csr.rdata
  val csrType = decodeOutput(parameter.decoderParameter.csrType)
  val CSR_NONE = parameter.decoderParameter.CSR_NONE.U(parameter.decoderParameter.CSR_TYPE_LEN.W)
  val CSR_MRET = parameter.decoderParameter.CSR_MRET.U(parameter.decoderParameter.CSR_TYPE_LEN.W)
  io.csr.csr_addr := Mux(csrType===CSR_MRET, 0x341.U(12.W), inst(31, 20)) // TODO replace magic numvber
  io.csr.wen := Mux(csrType === CSR_NONE, false.B, true.B)
  io.csr.wdata := alu.io.out
  io.out.bits.csr_data := csr_data
  io.bypass_out.epc := csr_data

  // ALU 
  alu.io.alu_op := decodeOutput(parameter.decoderParameter.aluFn)
  alu.io.A := MuxLookup(decodeOutput(parameter.decoderParameter.selAlu1), rs1)(
    Seq(
      parameter.decoderParameter.ALU1_PC.U -> pc,
      parameter.decoderParameter.ALU1_RS1.U -> rs1,
      parameter.decoderParameter.ALU1_ZERO.U -> 0.U(xlen.W),
      parameter.decoderParameter.ALU1_CSR.U -> csr_data
    )
  )
  alu.io.B := MuxLookup(decodeOutput(parameter.decoderParameter.selAlu2), rs2)(
    Seq(
      parameter.decoderParameter.ALU2_IMM.U -> imm_out,
      parameter.decoderParameter.ALU2_RS2.U -> rs2,
      parameter.decoderParameter.ALU2_RS1.U -> rs1,
      parameter.decoderParameter.ALU2_ZERO.U -> 0.U(xlen.W)
    )
  )
  io.bypass_out.alu_out := alu.io.out

  // Branch condition 
  val brCond: Instance[BrCond] = Instantiate(new BrCond(parameter.brCondParameter))
  brCond.io.rs1 := rs1
  brCond.io.rs2 := rs2
  brCond.io.br_type := decodeOutput(parameter.decoderParameter.brType)
  io.bypass_out.taken := brCond.io.taken


  // connect stage reg
  io.out.bits.inst := inst
  io.out.bits.pc := pc
  io.out.bits.alu_out := alu.io.out
  io.out.bits.st_data := rs2
  io.out.bits.ld_type := decodeOutput(parameter.decoderParameter.ldType) 
  io.out.bits.st_type := decodeOutput(parameter.decoderParameter.stType)
  io.out.bits.wb_sel := decodeOutput(parameter.decoderParameter.selWB)

  // handshake protocol
  // TODO maybe wrong when add mux or div
  io.in.ready := io.out.ready
  io.out.valid := io.in.valid
}

object WriteBackStageParameter {
  implicit def rwP: upickle.default.ReadWriter[WriteBackStageParameter] =
    upickle.default.macroRW[WriteBackStageParameter]
}

case class WriteBackStageParameter(xlen: Int, decoderParameter: DecoderParameter) extends SerializableModuleParameter {
}

class WriteBackStageInterface(parameter: WriteBackStageParameter) extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())

  val in = Flipped(Decoupled(new ExecuteStageMessage(parameter.xlen, parameter.decoderParameter)))
  val gpr_write = Flipped(new regWriteIO(parameter.xlen))
  val dcache = Flipped(new DCacheIO(parameter.xlen))
  val bypass = new WriteBackStageBypassMessage(parameter.xlen, parameter.decoderParameter.WB_SEL_LEN)
}

@instantiable
class WriteBackStage(val parameter: WriteBackStageParameter)
    extends FixedIORawModule(new WriteBackStageInterface(parameter))
    with SerializableModule[WriteBackStageParameter]
    with Public 
    with ImplicitClock
    with ImplicitReset {
  override protected def implicitClock: Clock = io.clock
  override protected def implicitReset: Reset = io.reset
  val xlen = parameter.xlen
  
  val inst = io.in.bits.inst
  val pc = io.in.bits.pc
  val rd = inst(11, 7) // TODO maybe use 5 bits reg instead of 32 bits

   // dcache connnect
  val dcache_wen = io.in.bits.st_type =/= parameter.decoderParameter.ST_NONE.U
  val dcache_ren =  io.in.bits.st_type =/= parameter.decoderParameter.LD_NONE.U
  val rdata = io.dcache.rdata
  io.dcache.raddr := io.in.bits.alu_out
  io.dcache.ren := dcache_ren
  io.dcache.waddr := io.in.bits.alu_out
  io.dcache.wen := dcache_wen
  io.dcache.wdata := io.in.bits.st_data
  io.dcache.wmask := MuxLookup(io.in.bits.st_type, "b0000".U)( // TODO use args replace magic number 4, or select 4/8
    Seq(
      parameter.decoderParameter.ST_SW.U -> "b1111".U(4.W),
      parameter.decoderParameter.ST_SH.U -> "b11".U(4.W),
      parameter.decoderParameter.ST_SB.U -> "b1".U(4.W) 
    )
  )

  // gpr connnect
  val loffset = Cat(io.in.bits.alu_out(1), io.in.bits.alu_out(0), 0.U(2.W))
  val lshift = rdata >> loffset
  val load = MuxLookup(io.in.bits.ld_type, rdata.zext)(
    Seq(
      parameter.decoderParameter.LD_LH.U -> lshift(15, 0).asSInt,
      parameter.decoderParameter.LD_LB.U -> lshift(7, 0).asSInt,
      parameter.decoderParameter.LD_LHU.U -> lshift(15, 0).zext,
      parameter.decoderParameter.LD_LBU.U -> lshift(7, 0).zext
    )
  )
  val gprWrite = MuxLookup(io.in.bits.wb_sel, io.in.bits.alu_out.zext)(
    Seq(
      parameter.decoderParameter.WB_MEM.U -> load,
      parameter.decoderParameter.WB_ALU.U -> io.in.bits.alu_out.zext,
      parameter.decoderParameter.WB_PC4.U -> (pc + 4.U).zext,
      parameter.decoderParameter.WB_CSR.U -> io.in.bits.csr_data.zext
    )
  ).asUInt
  io.gpr_write.req.waddr := rd
  io.gpr_write.req.wdata := gprWrite
  val gpr_wen = io.in.bits.wb_sel =/= parameter.decoderParameter.WB_NONE.U
  io.gpr_write.req.wen := gpr_wen

  // bypass connect
  io.bypass.wb_sel := io.in.bits.wb_sel
  io.bypass.wb_en := gpr_wen
  io.bypass.wb_rd_addr := rd
  io.bypass.wb_data := gprWrite

  // TODO handshake procotol
  val s_idle :: s_load :: s_store :: s_writeback :: Nil  = Enum(4)
  val state = RegInit(s_idle)
  state := MuxLookup(state, s_idle)(Seq(
    s_idle -> Mux(io.in.valid, MuxCase(s_writeback, Seq(dcache_wen->s_load, dcache_ren->s_load)), s_idle),
    s_load -> s_writeback,
    s_store -> Mux(io.in.valid, MuxCase(s_writeback, Seq(dcache_wen->s_load, dcache_ren->s_load)), s_idle),
    s_writeback -> Mux(io.in.valid, MuxCase(s_writeback, Seq(dcache_wen->s_load, dcache_ren->s_load)), s_idle),
  ))
  io.in.ready := MuxLookup(state, true.B)(Seq(
    s_idle -> true.B,
    s_load -> false.B,
    s_store -> true.B,
    s_writeback -> true.B
  ))
}

object CoreParameter {
  implicit def rwP: upickle.default.ReadWriter[CoreParameter] =
    upickle.default.macroRW[CoreParameter]
}

case class CoreParameter() extends SerializableModuleParameter {
  def xlen: Int = 32 // TODO depend on instructionSets

  val decoderParameter = DecoderParameter()
  val fetchstageParameter = FetchStageParameter(xlen, decoderParameter)
  val executestageParameter = ExecuteStageParameter(xlen, decoderParameter)
  val writebackstageParameter = WriteBackStageParameter(xlen, decoderParameter)
  val csrParameter = CSRParameter(xlen)
}

class CoreInterface(parameter: CoreParameter) extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())
  
  val icache = Flipped(new ICacheIO(parameter.xlen))
  val dcache = Flipped(new DCacheIO(parameter.xlen))
}



@instantiable
class Core(val parameter: CoreParameter)
    extends FixedIORawModule(new CoreInterface(parameter))
    with SerializableModule[CoreParameter]
    with Public
    with ImplicitClock
    with ImplicitReset {
  override protected def implicitClock: Clock = io.clock
  override protected def implicitReset: Reset = io.reset

  def pipelineConnect[T <: Data, T2 <: Data](prevOut: DecoupledIO[T], thisIn: DecoupledIO[T2]) = {
    prevOut.ready := thisIn.ready
    thisIn.valid := prevOut.valid
    thisIn.bits := RegEnable(prevOut.bits, prevOut.valid && thisIn.ready)
  }

  val xlen = parameter.xlen
  val fetchStage = Instantiate(new FetchStage(parameter.fetchstageParameter))
  val executeStage = Instantiate(new ExecuteStage(parameter.executestageParameter))
  val writebackStage = Instantiate(new WriteBackStage(parameter.writebackstageParameter))
  val gpr = Module(new RegFile(xlen))
  val csr: Instance[CSR] = Instantiate(new CSR(parameter.csrParameter))


  fetchStage.io.clock := io.clock
  executeStage.io.clock := io.clock
  writebackStage.io.clock := io.clock
  fetchStage.io.reset := io.reset
  executeStage.io.reset := io.reset
  writebackStage.io.reset := io.reset
  csr.io.clock := io.clock
  csr.io.reset := io.reset


  pipelineConnect(fetchStage.io.out, executeStage.io.in)
  pipelineConnect(executeStage.io.out, writebackStage.io.in)

  fetchStage.io.icache <> io.icache
  fetchStage.io.bypass <> executeStage.io.bypass_out
  executeStage.io.inst := io.icache.rdata
  executeStage.io.bypass_in <> writebackStage.io.bypass
  executeStage.io.gpr_read <> gpr.io.r
  executeStage.io.csr <> csr.io.csr
  writebackStage.io.gpr_write <> gpr.io.w
  writebackStage.io.dcache <> io.dcache
}


class RegFile(xlen: Int) extends Module {
  val io = IO(new RegFileIO(xlen))
  val regs = Mem(32, UInt(xlen.W))
  io.r.rsp.data1 := Mux(io.r.req.addr1.orR, regs(io.r.req.addr1), 0.U)
  io.r.rsp.data2 := Mux(io.r.req.addr2.orR, regs(io.r.req.addr2), 0.U)
  when(io.w.req.wen & io.w.req.waddr.orR) {
    regs(io.w.req.waddr) := io.w.req.wdata
  }
}