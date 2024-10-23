package org.chipsalliance.hia

import chisel3._
import chisel3.util._
import chisel3.experimental.hierarchy.{instantiable, public, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}

object ALUParameter {
  implicit def rwP: upickle.default.ReadWriter[ALUParameter] =
    upickle.default.macroRW[ALUParameter]
}

case class ALUParameter(xlen: Int, decoderParameter: DecoderParameter) extends SerializableModuleParameter {
  val ALU_ADD = decoderParameter.ALU_ADD
  val ALU_SUB = decoderParameter.ALU_SUB
  val ALU_AND = decoderParameter.ALU_AND
  val ALU_OR = decoderParameter.ALU_OR
  val ALU_XOR = decoderParameter.ALU_XOR
  val ALU_SLT = decoderParameter.ALU_SLT
  val ALU_SLL = decoderParameter.ALU_SLL
  val ALU_SLTU =decoderParameter.ALU_SLTU
  val ALU_SRL = decoderParameter.ALU_SRL
  val ALU_SRA = decoderParameter.ALU_SRA
}

class ALUInterface(parameter: ALUParameter) extends Bundle {
  val A = Input(UInt(parameter.xlen.W))
  val B = Input(UInt(parameter.xlen.W))
  val alu_op = Input(UInt(4.W))
  val out = Output(UInt(parameter.xlen.W))
}

@instantiable
class ALU(val parameter: ALUParameter)
    extends FixedIORawModule(new ALUInterface(parameter))
    with SerializableModule[ALUParameter]
    with Public {
  val ALU_ADD = parameter.ALU_ADD
  val ALU_SUB = parameter.ALU_SUB
  val ALU_AND = parameter.ALU_AND
  val ALU_OR = parameter.ALU_OR
  val ALU_XOR = parameter.ALU_XOR
  val ALU_SLT = parameter.ALU_SLT
  val ALU_SLL = parameter.ALU_SLL
  val ALU_SLTU = parameter.ALU_SLTU
  val ALU_SRL = parameter.ALU_SRL
  val ALU_SRA = parameter.ALU_SRA

  val shamt = io.B(4, 0).asUInt

  io.out := MuxLookup(io.alu_op, io.B)(
    Seq(
      ALU_ADD -> (io.A + io.B),
      ALU_SUB -> (io.A - io.B),
      ALU_AND -> (io.A & io.B),
      ALU_OR -> (io.A | io.B),
      ALU_XOR -> (io.A ^ io.B),
      ALU_SLL -> (io.A << shamt),
      ALU_SLT -> (io.A.asSInt < io.B.asSInt),
      ALU_SLTU -> (io.A < io.B),
      ALU_SRL -> (io.A >> shamt),
      ALU_SRA -> (io.A.asSInt >> shamt).asUInt,
    )
  )
}
