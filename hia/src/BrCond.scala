package org.chipsalliance.hia

import chisel3._
import chisel3.util._
import chisel3.experimental.hierarchy.{instantiable, public, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}

// import org.chipsalliance.hia.Cache.{DCacheIO, ICacheIO}

object BrCondParameter {
  implicit def rwP: upickle.default.ReadWriter[BrCondParameter] =
    upickle.default.macroRW[BrCondParameter]
}

case class BrCondParameter(xlen: Int, ctrlParameter: ControlParameter) extends SerializableModuleParameter {}

class BrCondInterface(parameter: BrCondParameter) extends Bundle {
  val rs1 = Input(UInt(parameter.xlen.W))
  val rs2 = Input(UInt(parameter.xlen.W))
  val br_type = Input(UInt(3.W))
  val taken = Output(Bool())
}

@instantiable
class BrCond(val parameter: BrCondParameter)
    extends FixedIORawModule(new BrCondInterface(parameter))
    with SerializableModule[BrCondParameter]
    with Public {
  val BR_XXX = parameter.ctrlParameter.BR_XXX
  val BR_LTU = parameter.ctrlParameter.BR_LTU
  val BR_LT = parameter.ctrlParameter.BR_LT
  val BR_EQ = parameter.ctrlParameter.BR_EQ
  val BR_GEU = parameter.ctrlParameter.BR_GEU
  val BR_GE = parameter.ctrlParameter.BR_GE
  val BR_NE = parameter.ctrlParameter.BR_NE

  val eq = io.rs1 === io.rs2
  val neq = !eq
  val lt = io.rs1.asSInt < io.rs2.asSInt
  val ge = !lt
  val ltu = io.rs1 < io.rs2
  val geu = !ltu
  io.taken :=
    ((io.br_type === BR_EQ) && eq) ||
      ((io.br_type === BR_NE) && neq) ||
      ((io.br_type === BR_LT) && lt) ||
      ((io.br_type === BR_GE) && ge) ||
      ((io.br_type === BR_LTU) && ltu) ||
      ((io.br_type === BR_GEU) && geu)
}
