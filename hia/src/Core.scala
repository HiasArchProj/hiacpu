package org.chipsalliance.hia

import chisel3._
import chisel3.util._
import chisel3.experimental.hierarchy.{instantiable, public, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}

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
    with Public 
    with ImplicitClock
    with ImplicitReset {
  override protected def implicitClock: Clock = io.clock
  override protected def implicitReset: Reset = io.reset 
  val xlen = parameter.xlen

  val pc = RegInit((parameter.PC_START-4).U(xlen.W))
  val next_pc = MuxCase(
    (pc+4).U,
    Seq(
      io.taken -> Cat(io.alu_out(xlen - 1, 1), 0.U(1.W)),
      ~io.out.ready -> pc
    )
  )

  // connect cache and stage reg
  io.next_pc := next_pc
  io.out.pc := next_pc
  io.out.inst := io.inst

  val inst_wait :: inst_ready :: Nil = Enum(2)
  val inst_state = RegInit(inst_wait)
  inst_state := MuxCase(
    inst_wait, 
    Seq(
      inst_wait -> inst_ready,
      inst_ready -> inst_wait
    )
  )

  io.out.valid := inst_state // FIXME maybe wrong
}



object ExecuteStageParameter {
  implicit def rwP: upickle.default.ReadWriter[ExecuteStageParameter] =
    upickle.default.macroRW[ExecuteStageParameter]
}

case class ExecuteStageParameter(xlen: Int, decoderParameter: DecoderParameter) extends SerializableModuleParameter {
  
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
  val raddr1 = Output(UInt(5.W))
  val rdata1 = Input(UInt(parameter.xlen.W))
}


object WriteBackStageParameter {
  implicit def rwP: upickle.default.ReadWriter[WriteBackStageParameter] =
    upickle.default.macroRW[WriteBackStageParameter]
}

case class WriteBackStageParameter(xlen: Int, decoderParameter: DecoderParameter) extends SerializableModuleParameter {
}

class WriteBackStageInterface(parameter: WriteBackStageParameter) extends Bundle {
  val raddr2 = Output(UInt(5.W))
  val rdata2 = Input(UInt(parameter.xlen.W))
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