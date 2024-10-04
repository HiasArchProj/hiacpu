package org.chipsalliance.hia

import chisel3._
import chisel3.util._
import chisel3.experimental.hierarchy.{instantiable, public, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}

// import org.chipsalliance.hia.Cache.{DCacheIO, ICacheIO}

object ALUParameter {
  implicit def rwP: upickle.default.ReadWriter[ALUParameter] =
    upickle.default.macroRW[ALUParameter]
}

case class ALUParameter(xlen: Int) extends SerializableModuleParameter {
  val ALU_ADD = 0.U(4.W)
  val ALU_SUB = 1.U(4.W)
  val ALU_AND = 2.U(4.W)
  val ALU_OR = 3.U(4.W)
  val ALU_XOR = 4.U(4.W)
  val ALU_SLT = 5.U(4.W)
  val ALU_SLL = 6.U(4.W)
  val ALU_SLTU = 7.U(4.W)
  val ALU_SRL = 8.U(4.W)
  val ALU_SRA = 9.U(4.W)
  val ALU_COPY_A = 10.U(4.W)
  val ALU_COPY_B = 11.U(4.W)
  val ALU_XXX = 15.U(4.W)
}

class ALUInterface(parameter: ALUParameter) extends Bundle {
  val A = Input(UInt(parameter.xlen.W))
  val B = Input(UInt(parameter.xlen.W))
  val alu_op = Input(UInt(4.W))
  val out = Output(UInt(parameter.xlen.W))
  val sum = Output(UInt(parameter.xlen.W))
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
  val ALU_COPY_A = parameter.ALU_COPY_A
  val ALU_XXX = parameter.ALU_XXX

  val shamt = io.B(4, 0).asUInt

  io.out := MuxLookup(io.alu_op, io.B)(
    Seq(
      ALU_ADD -> (io.A + io.B),
      ALU_SUB -> (io.A - io.B),
      ALU_SRA -> (io.A.asSInt >> shamt).asUInt,
      ALU_SRL -> (io.A >> shamt),
      ALU_SLL -> (io.A << shamt),
      ALU_SLT -> (io.A.asSInt < io.B.asSInt),
      ALU_SLTU -> (io.A < io.B),
      ALU_AND -> (io.A & io.B),
      ALU_OR -> (io.A | io.B),
      ALU_XOR -> (io.A ^ io.B),
      ALU_COPY_A -> io.A
    )
  )

  io.sum := io.A + Mux(io.alu_op(0), -io.B, io.B)
}
