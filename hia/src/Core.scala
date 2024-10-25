package org.chipsalliance.hia

import chisel3._
import chisel3.util._
import chisel3.experimental.hierarchy.{instantiable, public, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import os.stat

object FetchStageParameter {
  implicit def rwP: upickle.default.ReadWriter[FetchStageParameter] =
    upickle.default.macroRW[FetchStageParameter]
}

case class FetchStageParameter(xlen: Int) extends SerializableModuleParameter {
  val PC_START = 0x8000_0000
}

class FetchStageData(xlen: Int) extends Bundle {
  val inst = Output(UInt(xlen.W))
  val pc = Output(UInt(xlen.W))
}

class FetchStageInterface(parameter: FetchStageParameter) extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())

  val out = Decoupled(new FetchStageData(parameter.xlen))

  // TODO add handshaking protocol 
  val next_pc = Output(UInt(parameter.xlen.W))
  val inst = Input(UInt(parameter.xlen.W))

  // TODO add decoder signal or interrupt
  val taken = Input(Bool())
  val alu_out = Input(UInt(parameter.xlen.W))
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

  val pc = RegInit((parameter.PC_START-4).U(xlen.W))

  val s_idle :: s_wait_ready :: Nil = Enum(2)
  val state = RegInit(s_idle)
  state := MuxLookup(state, s_idle)(Seq(
    s_idle -> Mux(io.out.valid & ~io.out.ready, s_wait_ready, s_idle),
    s_wait_ready -> Mux(io.out.ready, s_idle, s_wait_ready)
  ))

  io.out.valid := true.B // FIXME maybe wrong

  val next_pc = MuxCase(
    (pc+4.U),
    Seq(
      io.taken -> Cat(io.alu_out(xlen - 1, 1), 0.U(1.W)),
      (state === s_wait_ready) -> pc
    )
  )
  pc := next_pc

  // connect cache and stage reg
  io.next_pc := next_pc
  io.out.bits.pc := next_pc
  io.out.bits.inst := io.inst
}



object ExecuteStageParameter {
  implicit def rwP: upickle.default.ReadWriter[ExecuteStageParameter] =
    upickle.default.macroRW[ExecuteStageParameter]
}

case class ExecuteStageParameter(xlen: Int, decoderParameter: DecoderParameter) extends SerializableModuleParameter {
  val aluParameter = ALUParameter(xlen, decoderParameter)
  val brCondParameter = BrCondParameter(xlen, decoderParameter)
}

class ExecuteStageData(xlen: Int, decoderParameter: DecoderParameter) extends Bundle {
  val inst = Output(UInt(xlen.W))
  val pc = Output(UInt(xlen.W))
  val alu_out = Output(UInt(xlen.W))
  val st_data = Output(UInt(xlen.W))

  val ld_type = Output(UInt(decoderParameter.LD_TYPE_LEN.W))
  val st_type = Output(UInt(decoderParameter.ST_TYPE_LEN.W))
  val wb_sel = Output(UInt(decoderParameter.WB_SEL_LEN.W))
}

class ExecuteStageInterface(parameter: ExecuteStageParameter) extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())

  val out = Decoupled(new ExecuteStageData(parameter.xlen, parameter.decoderParameter))
  val in = Flipped(Decoupled(new FetchStageData(parameter.xlen)))
  val raddr1 = Output(UInt(5.W))
  val rdata1 = Input(UInt(parameter.xlen.W))
  val raddr2 = Output(UInt(5.W))
  val rdata2 = Input(UInt(parameter.xlen.W))
  val taken = Output(Bool())
  val alu_out = Output(UInt(parameter.xlen.W))

  // bypass signal
  val wb_sel = Input(UInt(parameter.decoderParameter.WB_SEL_LEN.W))
  val wb_en = Input(Bool())
  val wb_rd_addr = Input(UInt(5.W))
  val wb_data = Input(UInt(parameter.xlen.W))
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
  val inst = io.in.bits.inst
  val pc = io.in.bits.pc

  val decoder: Instance[Decoder] = Instantiate(new Decoder(parameter.decoderParameter))
  val decodeOutput = Wire(chiselTypeOf(decoder.io.output))
  decoder.io.instruction := inst
  decodeOutput := decoder.io.output
  val alu: Instance[ALU] = Instantiate(new ALU(parameter.aluParameter))

  val imm_out = ImmGen(decodeOutput(parameter.decoderParameter.immType), inst)

  // read reg
  io.raddr1 := inst(19, 15)
  io.raddr2 := inst(24, 20)

  // bypass
  val rs1hazard = io.wb_en && io.raddr1.orR && (io.raddr1 === io.wb_rd_addr)
  val rs2hazard = io.wb_en && io.raddr2.orR && (io.raddr2 === io.wb_rd_addr)
  val rs1 = Mux(io.wb_sel === parameter.decoderParameter.WB_ALU.U && rs1hazard, io.wb_data, io.rdata1)
  val rs2 = Mux(io.wb_sel === parameter.decoderParameter.WB_ALU.U && rs2hazard, io.wb_data, io.rdata2)

  // ALU 
  alu.io.alu_op := decodeOutput(parameter.decoderParameter.aluFn)
  alu.io.A := MuxLookup(decodeOutput(parameter.decoderParameter.selAlu1), io.rdata1)(
    Seq(
      parameter.decoderParameter.ALU1_PC.U -> pc,
      parameter.decoderParameter.ALU1_RS1.U -> io.rdata1,
      parameter.decoderParameter.ALU1_ZERO.U -> 0.U(xlen.W)
    )
  )
  alu.io.B := MuxLookup(decodeOutput(parameter.decoderParameter.selAlu2), io.rdata2)(
    Seq(
      parameter.decoderParameter.ALU2_IMM.U -> imm_out,
      parameter.decoderParameter.ALU2_RS2.U -> io.rdata2,
      parameter.decoderParameter.ALU2_ZERO.U -> 0.U(xlen.W)
    )
  )
  io.alu_out := alu.io.out

  // Branch condition 
  val brCond: Instance[BrCond] = Instantiate(new BrCond(parameter.brCondParameter))
  brCond.io.rs1 := rs1
  brCond.io.rs2 := rs2
  brCond.io.br_type := decodeOutput(parameter.decoderParameter.brType)
  io.taken := brCond.io.taken

  // connect stage reg
  io.out.bits.inst := inst
  io.out.bits.pc := pc
  io.out.bits.alu_out := alu.io.out
  io.out.bits.st_data := rs2
  io.out.bits.ld_type := decodeOutput(parameter.decoderParameter.ldType) 
  io.out.bits.st_type := decodeOutput(parameter.decoderParameter.stType)
  io.out.bits.wb_sel := decodeOutput(parameter.decoderParameter.selWB)

  // handshake protocol
  val s_idle :: s_wait_ready :: s_wait_valid :: s_working :: Nil= Enum(4)
  val state = RegInit(s_idle)
  state := MuxLookup(state, s_idle)(
    Seq(
      s_idle -> MuxLookup(Cat(io.in.valid, io.out.ready), s_idle)(Seq(
                  "b10".U -> s_wait_ready,
                  "b01".U -> s_wait_valid,
                  "b11".U -> s_working
               )),
      s_wait_ready -> Mux(io.out.ready, s_working, s_wait_ready),
      s_wait_valid -> Mux(io.in.valid, s_working, s_wait_valid),
      s_working -> MuxCase(s_working, Seq(
                     ~io.out.ready -> s_wait_ready,
                     ~io.in.valid-> s_wait_valid,
                     (~io.in.valid && ~io.out.ready) -> s_idle
                  ))
    )
  )

   // FIXME check grammar correct?
  val out_list = ListLookup(state, List(false.B, false.B), Array(
    BitPat(s_idle) -> List(true.B, false.B),
    BitPat(s_wait_ready) -> List(false.B, true.B),
    BitPat(s_wait_valid) -> List(true.B, false.B),
    BitPat(s_working) -> List(true.B, true.B)
  ))

  io.in.ready := out_list(0) 
  io.out.valid := out_list(1) 
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

  val in = Flipped(Decoupled(new ExecuteStageData(parameter.xlen, parameter.decoderParameter)))

  val waddr = Output(UInt(5.W))
  val wdata = Output(UInt(parameter.xlen.W))
  val wen = Output(Bool())
  val dcache = Flipped(new DCacheIO(parameter.xlen))

  // bypass signal
  val wb_sel = Output(UInt(parameter.decoderParameter.WB_SEL_LEN.W))
  val wb_en = Output(Bool())
  val wb_rd_addr = Output(UInt(5.W))
  val wb_data = Output(UInt(parameter.xlen.W))
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
  val rd = inst(11, 7)

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
      parameter.decoderParameter.WB_PC4.U -> (pc + 4.U).zext
    )
  ).asUInt
  io.waddr := rd
  io.wdata := gprWrite
  val gpr_wen = io.in.bits.wb_sel =/= parameter.decoderParameter.WB_NONE.U
  io.wen := gpr_wen

  // bypass connect
  io.wb_sel := io.in.bits.wb_sel
  io.wb_en := gpr_wen
  io.wb_rd_addr := rd
  io.wb_data := gprWrite

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
  val fetchstageParameter = FetchStageParameter(xlen)
  val executestageParameter = ExecuteStageParameter(xlen, decoderParameter)
  val writebackstageParameter = WriteBackStageParameter(xlen, decoderParameter)
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

  val xlen = parameter.xlen
  val fetchStage = Instantiate(new FetchStage(parameter.fetchstageParameter))
  val executeStage = Instantiate(new ExecuteStage(parameter.executestageParameter))
  val writebackStage = Instantiate(new WriteBackStage(parameter.writebackstageParameter))
  val gpr = Module(new RegFile(xlen))

  // FIXME use RegEnable and modify handshake procotol
  val fe_regs = Reg(new FetchStageData(xlen))
  val ew_regs = Reg(new ExecuteStageData(xlen, parameter.decoderParameter))

  fetchStage.io.clock := io.clock
  executeStage.io.clock := io.clock
  writebackStage.io.clock := io.clock
  fetchStage.io.reset := io.reset
  executeStage.io.reset := io.reset
  writebackStage.io.reset := io.reset

  // FIXME connect may be wrong
  fe_regs <> fetchStage.io.out.bits 
  io.icache.raddr := fetchStage.io.next_pc
  fetchStage.io.inst := io.icache.rdata
  fetchStage.io.taken := executeStage.io.taken
  fetchStage.io.alu_out := executeStage.io.alu_out

  ew_regs <> executeStage.io.out.bits
  fe_regs <> executeStage.io.in.bits
  gpr.io.raddr1 := executeStage.io.raddr1 
  gpr.io.raddr2 := executeStage.io.raddr2
  executeStage.io.rdata1 := gpr.io.rdata1 
  executeStage.io.rdata2 := gpr.io.rdata2 
  executeStage.io.wb_sel := writebackStage.io.wb_sel 
  executeStage.io.wb_en := writebackStage.io.wb_en  
  executeStage.io.wb_rd_addr := writebackStage.io.wb_rd_addr 
  executeStage.io.wb_data := writebackStage.io.wb_data  

  ew_regs <> writebackStage.io.in.bits
  writebackStage.io.dcache <> io.dcache
  gpr.io.waddr := writebackStage.io.waddr 
  gpr.io.wdata := writebackStage.io.wdata  
  gpr.io.wen := writebackStage.io.wen 

  fetchStage.io.out.ready :=  executeStage.io.in.ready
  executeStage.io.in.valid :=  fetchStage.io.out.valid
  executeStage.io.out.ready := writebackStage.io.in.ready
  writebackStage.io.in.valid := executeStage.io.out.valid

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