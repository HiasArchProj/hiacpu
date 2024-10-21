package org.chipsalliance.hia

import chisel3._
import chisel3.experimental.hierarchy.{instantiable, public, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}

import chisel3.util.BitPat
import chisel3.util.experimental.decode.{BoolDecodeField, DecodeField, DecodePattern, DecodeTable}
import org.chipsalliance.rvdecoderdb.{Encoding, Instruction, InstructionSet}

object DecoderParameter {
  implicit def rwP: upickle.default.ReadWriter[DecoderParameter] = upickle.default.macroRW[DecoderParameter]
}

case class DecoderParameter() extends SerializableModuleParameter {
  private val instructionTable = org.chipsalliance.rvdecoderdb.instructions(org.chipsalliance.rvdecoderdb.extractResource(getClass.getClassLoader))
  private val targetSets = Set("rv_i", "rv32_i")
  private val instructionDecodePattern = instructionTable
    .filter(instr => targetSets.contains(instr.instructionSet.name))
    .filter(_.pseudoFrom.isEmpty)
    .map(HiaDecodePattern(_))
    .toSeq

  private val instructionDecodeFields = Seq(isBranch)

  val table: DecodeTable[HiaDecodePattern] = new DecodeTable[HiaDecodePattern](
    instructionDecodePattern,
    instructionDecodeFields
  )

  object isBranch extends BoolDecodeField[HiaDecodePattern] {
    override def name: String = "branch"

    override def genTable(op: HiaDecodePattern): BitPat = op.instruction.name match {
      case i if Seq("bne", "beq", "blt", "bltu", "bge", "bgeu").contains(i) => y
      case _                                                                => n
    }
  }
}

class DecoderInterface(parameter: DecoderParameter) extends Bundle {
  val instruction = Input(UInt(32.W))
  val output = Output(parameter.table.bundle)
}

@instantiable
class Decoder(val parameter: DecoderParameter) extends FixedIORawModule(new DecoderInterface(parameter)) with SerializableModule[DecoderParameter] with Public {
  io.output := parameter.table.decode(io.instruction)
}

case class HiaDecodePattern(val instruction: Instruction) extends DecodePattern {
  override def bitPat: BitPat = BitPat("b" + instruction.encoding.toString())
}
