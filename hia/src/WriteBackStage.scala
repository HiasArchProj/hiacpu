package org.chipsalliance.hia

import chisel3._
import chisel3.util._
import chisel3.experimental.hierarchy.{instantiable, public, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}

object WriteBackStageParameter {
  implicit def rwP: upickle.default.ReadWriter[WriteBackStageParameter] =
    upickle.default.macroRW[WriteBackStageParameter]
}

case class WriteBackStageParameter() extends SerializableModuleParameter {}

class WriteBackStageInterface(parameter: WriteBackStageParameter) extends Bundle {}

@instantiable
class WriteBackStage(parameter: WriteBackStageParameter)
    extends FixedIORawModule(new WriteBackStageInterface(parameter))
    with SerializableModule[WriteBackStageParameter]
    with Public {

  val REN_LEN = 2
  val REN_NONE = 0.U(REN_LEN.W)
  val REN_SCALAR = 1.U(REN_LEN.W) // Scalar

  def connect(prev: MemStage, gr: Mem[UInt]) {
    // FIXME change REN_SCALAR to REN?
    when(prev.reg_rf_wen === REN_SCALAR) {
      gr(prev.reg_wb_addr) := prev.reg_wb_data
    }
  }
}
