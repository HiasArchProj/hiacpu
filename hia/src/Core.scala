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
}

class FetchStageData(xlen: Int) extends Bundle {
  val inst = Output(UInt(xlen.W))
  val pc = Output(UInt(xlen.W))
}

class FetchStageInterface(parameter: FetchStageParameter) extends Bundle {
  val out = Decoupled(new FetchStageData(parameter.xlen))
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
}


object WriteBackStageParameter {
  implicit def rwP: upickle.default.ReadWriter[WriteBackStageParameter] =
    upickle.default.macroRW[WriteBackStageParameter]
}

case class WriteBackStageParameter(xlen: Int, decoderParameter: DecoderParameter) extends SerializableModuleParameter {
}

class WriteBackStageInterface(parameter: WriteBackStageParameter) extends Bundle {
  
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
