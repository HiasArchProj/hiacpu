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
  val out = Decoupled(new FetchStageData(parameter.xlen))

  // TODO add handshaking protocol 
  val next_pc = Output(UInt(5.W))
  val inst = Input(UInt(parameter.xlen.W))

  // TODO add decoder signal or interrupt
  val taken = Input(Bool())
  val alu_out = Input(UInt(parameter.xlen.W))
}

@instantiable
class FetchStage(val parameter: FetchStageParameter)
    extends FixedIORawModule(new FetchStageInterface(parameter))
    with SerializableModule[FetchStageParameter]
    with Public {
  val xlen = parameter.xlen

  val started = RegNext(io.reset.asBool)
  val pc = RegInit((parameter.PC_START-4).U(xlen.W))
  val next_pc = MuxCase(
    (pc+4).U,
    Seq(
      io.taken -> Cat(io.alu_out(xlen - 1, 1), 0.U(1.W)),
      (state === s_wait_ready) -> pc
    )
  )

  // connect cache and stage reg
  io.next_pc := next_pc
  io.out.pc := next_pc
  io.out.inst := io.inst

  val s_idle :: s_wait_ready :: Nul = Enum(2)
  val state = RegInit(s_idle)
  state := MuxLookup(state, s_idle)(Seq(
    s_idle -> Mux(io.out.valid, s_wait_ready, s_idle),
    s_wait_ready -> Mux(io.out.ready, s_idle, s_wait_ready)
  ))

  io.out.valid := true.B // FIXME maybe wrong
}



object ExecuteStageParameter {
  implicit def rwP: upickle.default.ReadWriter[ExecuteStageParameter] =
    upickle.default.macroRW[ExecuteStageParameter]
}

case class ExecuteStageParameter(xlen: Int, decoderParameter: DecoderParameter) extends SerializableModuleParameter {
  val aluParameter = ALUParameter(xlen, decoderParameter)
}

class ExecuteStageData(xlen: Int, decoderParameter: DecoderParameter) extends Bundle {
  val inst = Output(UInt(xlen.W))
  val pc = Output(UInt(xlen.W))
  val alu_out = Output(UInt(xlen.W))
  val st_data = Output(UInt(xlen.W))

  val ld_type = Output(UInt(decoderParameter.LD_TYPE_LEN.W))
  val st_type = Output(UInt(decoderParameter.ST_TYPE_LEN.W))
  val wb_type = Output(UInt(decoderParameter.WB_SEL_LEN.W))
}

class ExecuteStageInterface(parameter: ExecuteStageParameter) extends Bundle {
  val out = Decoupled(new ExecuteStageData(parameter.xlen, parameter.decoderParameter))
  val in = Flipped(Decoupled(new FetchStageData(parameter.xlen)))
  val raddr1 = Output(UInt(5.W))
  val rdata1 = Input(UInt(parameter.xlen.W))
  val raddr2 = Output(UInt(5.W))
  val rdata2 = Input(UInt(parameter.xlen.W))
  val taken = Output(Bool())
  val alu_out = Output(UInt(parameter.xlen.W))

  // bypass signal
  val wb_sel = Input(UInt(parameter.decoderParameter.WB_SEL_LEN))
  val wb_en = Input(Bool())
  val wb_rd_addr = Input(UInt(5.W))
  val wb_data = Input(UInt(parameter.xlen.W))
}

@instantiable
class ExecuteStage(val parameter: ExecuteStageParameter)
    extends FixedIORawModule(new ExecuteStageInterface(parameter))
    with SerializableModule[ExecuteStageParameter]
    with Public {

  val xlen = parameter.xlen
  val inst = io.in.bits.inst
  val pc = io.in.bits.pc

  val decoder: Instance[Decoder] = Instantiate(new Decoder(parameter.decoderParameter))
  val decodeOutput: DecodeBundle = Wire(chiselTypeOf(decoder.io.output))
  val alu: Instance[ALU] = Instantiate(new ALU(parameter.aluParameter))

  val imm_out = ImmGen(decodeOutput.imm_sel, inst)

  // read reg
  io.raddr1 := inst(19, 15)
  io.raddr2 := inst(24, 20)

  // bypass
  val rs1hazard = io.wb_en && rs1_addr.orR && (io.raddr1 === io.wb_rd_addr)
  val rs2hazard = io.wb_en && rs2_addr.orR && (io.raddr2 === io.wb_rd_addr)
  val rs1 = Mux(io.wb_sel === parameter.decoderParameter.WB_ALU && rs1hazard, wb_data, io.rdata1)
  val rs2 = Mux(io.wb_sel === parameter.decoderParameter.WB_ALU && rs2hazard, wb_data, io.rdata2)

  // ALU 
  alu.io.alu_op := decoderParameter.aluFn
  alu.io.A := MuxLookup(decodeOutput.sel_alu1, io.rdata1)(
    Seq(
      decoderParameter.ALU1_PC -> pc
      decoderParameter.ALU1_RS1 -> io.rdata1
      decoderParameter.ALU1_ZERO -> 0.U(xlen.W)
    )
  )
  alu.io.B := MuxLookup(decodeOutput.sel_alu2, io.rdata2)(
    Seq(
      decoderParameter.ALU2_PC -> pc
      decoderParameter.ALU2_RS1 -> io.rdata2
      decoderParameter.ALU2_ZERO -> 0.U(xlen.W)
    )
  )

  // Branch condition 
  val brCond: Instance[BrCond] = Instantiate(new BrCond(xlen, decoderParameter))
  brCond.io.rs1 := rs1
  brCond.io.rs2 := rs2
  brCond.io.br_type := decodeOutput.brType

  // connect stage reg
  io.out.bits.inst := inst
  io.out.bits.pc := pc
  io.out.bits.alu_out := alu.io.out
  io.out.bits.st_data := rs2
  io.out.bits.ld_type := decodeOutput.ldType 
  io.out.bits.st_type := decodeOutput.stType
  io.out.bits.wb_type := decodeOutput.selWB

  // handshake protocol
  val s_idle :: s_wait_ready :: s_wait_valid :: s_working = Enum(4)
  val state = RegInit(s_idle)
  state := MuxLookup(state, s_idle)(
    Seq(
      s_idle -> MuxLookup((io.in.valid, io.out.ready), s_idle)(Seq(
                  (true.B, false.B) -> s_wait_ready,
                  (false.B, true.B) -> s_wait_valid,
                  (true.B, true.B) -> s_working
               )),
      s_wait_ready -> Mux(io.out.ready, s_working, s_wait_ready),
      s_wait_valid -> Mux(io.in.valid, s_working, s_wait_valid),
      s_working => MuxCase(s_working, Seq(
                     ~io.out.ready -> s_wait_ready,
                     ~io.in.valid-> s_wait_valid,
                     (~io.in.valid && ~io.out.ready) -> s_idle
                  ))
    )
  )

  val (io.in.ready, io.out.valid) = MuxLookup(state, (false.B, false.B))(Seq(
    s_idle -> (true.B, false.B),
    s_wait_ready -> (false.B, true.B),
    s_wait_valid -> (true.B, false.B),
    s_working -> (true.B, true.B)
  ))
}

object WriteBackStageParameter {
  implicit def rwP: upickle.default.ReadWriter[WriteBackStageParameter] =
    upickle.default.macroRW[WriteBackStageParameter]
}

case class WriteBackStageParameter(xlen: Int, decoderParameter: DecoderParameter) extends SerializableModuleParameter {
}

class WriteBackStageInterface(parameter: WriteBackStageParameter) extends Bundle {
  val rwen = Output(Bool())
  val rwaddr = Output(UInt(5.W))
  val rwdata = Input(UInt(parameter.xlen.W))

  // TODO add handshaking protocol 
  val caddr = Output(UInt(parameter.xlen.W))
  val crdata = Output(UInt(parameter.xlen.W))
  val cwdata = Output(UInt(parameter.xlen.W))
  val cwen = Output(Bool())
  val cwmask = Output(UInt((parameter.xlen / 8).W))
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

  val dpath = Instantiate(new Datapath(parameter.datapathParameter))
  val ctrl = Instantiate(new Control(parameter.ctrlparameter))
  io.dcache <> dpath.io.dcache
  io.icache <> dpath.io.icache
  ctrl.io <> dpath.io.ctrl

  dpath.io.clock := io.clock
  dpath.io.reset := io.reset
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