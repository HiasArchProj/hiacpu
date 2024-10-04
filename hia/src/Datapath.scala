package org.chipsalliance.hia

import chisel3._
import chisel3.util._
import chisel3.experimental.hierarchy.{instantiable, public, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}

object DatapathParameter {
  implicit def rwP: upickle.default.ReadWriter[DatapathParameter] =
    upickle.default.macroRW[DatapathParameter]
}

case class DatapathParameter(xlen: Int, ctrl: ControlParameter) extends SerializableModuleParameter {}

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

}
