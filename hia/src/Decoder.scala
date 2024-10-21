package org.chipsalliance.hia

import chisel3._
import chisel3.experimental.hierarchy.{instantiable, public, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}

import chisel3.util.BitPat
import chisel3.util.experimental.decode.{BoolDecodeField, DecodeField, DecodePattern, DecodeTable}
import org.chipsalliance.rvdecoderdb.{Encoding, Instruction, InstructionSet}

trait UOP {
  def width: Int

  def dontCare: BitPat = BitPat.dontCare(width)

  def chiselType: TPE = UInt(width.W)

  def encode(lit: Int): BitPat = BitPat(lit.U(width.W))

  def encode(strLit: String): BitPat = BitPat(strLit.U(width.W))

  type TPE = UInt
}

trait UOPDecodeField[T <: DecodePattern] extends DecodeField[T, UInt] {
  def uopType: UOP

  def chiselType: UInt = uopType.chiselType
}

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

  private val instructionDecodeFields = Seq(immType, brType, selAlu1, selAlu2, aluFn, stType, ldType, selWB)

  val table: DecodeTable[HiaDecodePattern] = new DecodeTable[HiaDecodePattern](
    instructionDecodePattern,
    instructionDecodeFields
  )

  object UOPIMM extends UOP {
    def width = 3

    def s: BitPat = encode(0)

    def sb: BitPat = encode(1)

    def u: BitPat = encode(2)

    def uj: BitPat = encode(3)

    def i: BitPat = encode(4)

    def z: BitPat = encode(5)
  }

  object immType extends  UOPDecodeField[HiaDecodePattern] {
    override def name: String = "imm_type"

    override def genTable(op: HiaDecodePattern): BitPat = op.instruction.name match {
      // format: off
      case i if Seq("fld", "flw", "hsv.w", "hsv.b", "hsv.h", "hsv.d", "ori", "lhu", "lw", "andi", "sltiu", "lh", "jalr", "lbu", "xori", "slti", "addi", "lb", "srli", "srai", "slli", "ld", "sraiw", "lwu", "addiw", "srliw", "slliw", "flh").contains(i) => UOPIMM.i
      case i if Seq("fsd", "fsh", "fsw", "sb", "sd", "sh", "sw").contains(i) => UOPIMM.s
      case i if Seq("beq", "bge", "bgeu", "blt", "bltu", "bne").contains(i) => UOPIMM.sb
      case i if Seq("auipc", "lui").contains(i) => UOPIMM.u
      case i if Seq("jal").contains(i) => UOPIMM.uj
      case i if Seq("csrrci", "csrrsi", "csrrwi").contains(i) => UOPIMM.z
      case _ => UOPIMM.dontCare
      // format: on
    }

    override def uopType: UOPIMM.type = UOPIMM
  }

  object UOPBR extends UOP {
    def width = 3

    def ltu: BitPat = encode(0)

    def lt: BitPat = encode(1)

    def eq: BitPat = encode(2)

    def geu: BitPat = encode(3)

    def ge: BitPat = encode(4)

    def ne: BitPat = encode(5)
  }

  object brType extends  UOPDecodeField[HiaDecodePattern] {
    override def name: String = "br_type"

    override def genTable(op: HiaDecodePattern): BitPat = op.instruction.name match {
      case "bltu" => UOPBR.ltu
      case "blt" => UOPBR.lt
      case "beq" => UOPBR.eq
      case "bgeu" => UOPBR.geu
      case "bge" => UOPBR.ge
      case "bne" => UOPBR.ne
      case _ => UOPBR.dontCare
    }

    override def uopType: UOPBR.type = UOPBR
  }

  object UOPA1 extends UOP {
    def width = 2

    def pc: BitPat = encode(0)

    def rs1: BitPat = encode(1)

    def zero: BitPat = encode(2)
  }

  object selAlu1 extends  UOPDecodeField[HiaDecodePattern] {
    override def name: String = "sel_alu1"

    override def genTable(op: HiaDecodePattern): BitPat = op.instruction.name match {
      case i if Seq("auipc", "jal", "beq", "bne", "blt", "bge", "bltu", "bgeu").contains(i) => UOPA1.pc
      case i if Seq("jalr",  "lb", "lh", "lw", "lbu", "lhu", "sb", "sh", "sw", "addi", "slti", "sltiu", "xori", "ori", "andi", "add", "sub", "sll", "slt", "sltu", "xor", "srl", "sra", "or", "and").contains(i) => UOPA1.rs1
      case i if Seq("lui").contains(i) => UOPA1.zero
      case _ => UOPA1.dontCare
    }
    override def uopType: UOPA1.type = UOPA1
  }

  object UOPA2 extends UOP {
    def width = 2

    def imm: BitPat = encode(0)

    def rs2: BitPat = encode(1)

    def zero: BitPat = encode(2)
  }
  object selAlu2 extends  UOPDecodeField[HiaDecodePattern] {
    override def name: String = "sel_alu2"

    override def genTable(op: HiaDecodePattern): BitPat = op.instruction.name match {
      case i if Seq("lui", "auipc", "jalr", "jal", "beq", "bne", "blt", "bge", "bltu", "bgeu", "lb", "lh", "lw", "lbu", "lhu", "sb", "sh", "sw", "addi", "slti", "sltiu", "xori", "ori", "andi").contains(i) => UOPA2.imm
      case i if Seq("add", "sub", "sll", "slt", "sltu", "xor", "srl", "sra", "or", "and").contains(i) => UOPA2.rs2
      case _ => UOPA2.dontCare
    }
    override def uopType: UOPA2.type = UOPA2
  }


  object UOPALU extends UOP {
    def width = 4

    def add: BitPat = encode(0)
    def sub: BitPat = encode(1)
    def and: BitPat = encode(2)
    def or: BitPat = encode(3)
    def xor: BitPat = encode(4)
    def slt: BitPat = encode(5)
    def sll: BitPat = encode(6)
    def sltu: BitPat = encode(7)
    def srl: BitPat = encode(8)
    def sra: BitPat = encode(9)
  }

  object aluFn extends  UOPDecodeField[HiaDecodePattern] {
    override def name: String = "aluFn"

    override def genTable(op: HiaDecodePattern): BitPat = op.instruction.name match {
      case i if Seq("lui", "auipc", "jalr", "jal", "beq", "bne", "blt", "bge", "bltu", "bgeu", "lb", "lh", "lw", "lbu", "lhu", "sb", "sh", "sw", "addi", "add").contains(i) => UOPALU.add
      case i if Seq("sub").contains(i) => UOPALU.sub
      case i if Seq("andi", "and").contains(i) => UOPALU.and
      case i if Seq("ori", "or").contains(i) => UOPALU.or
      case i if Seq("xori", "xor").contains(i) => UOPALU.xor
      case i if Seq("slti", "slt").contains(i) => UOPALU.slt
      case i if Seq("slli", "sll").contains(i) => UOPALU.sll
      case i if Seq("sltiu", "sltu").contains(i) => UOPALU.sltu
      case i if Seq("srli", "srl").contains(i) => UOPALU.srl
      case i if Seq("srai", "sra").contains(i) => UOPALU.sra
      case _ => UOPALU.dontCare
    }

    override def uopType: UOPALU.type = UOPALU
  }

  object UOPST extends UOP {
    def width = 2

    def sw: BitPat = encode(0)

    def sh: BitPat = encode(1)

    def sb: BitPat = encode(2)
  }

  object stType extends  UOPDecodeField[HiaDecodePattern] {
    override def name: String = "st_type"

    override def genTable(op: HiaDecodePattern): BitPat = op.instruction.name match {
      case i if Seq("sw").contains(i) => UOPST.sw
      case i if Seq("sh").contains(i) => UOPST.sh
      case i if Seq("sb").contains(i) => UOPST.sb
      case _ => UOPST.dontCare
    }

    override def uopType: UOPST.type = UOPST
  }

  object UOPLD extends UOP {
    def width = 3

    def lw: BitPat = encode(0)

    def lh: BitPat = encode(1)

    def lb: BitPat = encode(2)

    def lhu: BitPat = encode(3)

    def lbu: BitPat = encode(4)
  }
  object ldType extends  UOPDecodeField[HiaDecodePattern] {
    override def name: String = "ld_type"

    override def genTable(op: HiaDecodePattern): BitPat = op.instruction.name match {
      case i if Seq("lw").contains(i) => UOPLD.lw
      case i if Seq("lh").contains(i) => UOPLD.lh
      case i if Seq("lb").contains(i) => UOPLD.lb
      case i if Seq("lhu").contains(i) => UOPLD.lhu
      case i if Seq("lbu").contains(i) => UOPLD.lbu
      case _ => UOPLD.dontCare
    }

    override def uopType: UOPLD.type = UOPLD
  }

    object UOPWB extends UOP {
    def width = 2

    def alu: BitPat = encode(0)

    def mem: BitPat = encode(1)

    def pc4: BitPat = encode(2)
  }

  object selWB extends  UOPDecodeField[HiaDecodePattern] {
    override def name: String = "sel_wb"

    override def genTable(op: HiaDecodePattern): BitPat = op.instruction.name match {
      case i if Seq("lui", "auipc", "beq", "bne", "blt", "bge", "bltu", "bgeu", "sb", "sh", "sw", "addi", "slti", "sltiu", "xori", "ori", "andi", "add", "sub", "sll", "slt", "sltu", "xor", "srl", "sra", "or", "and").contains(i) => UOPWB.alu
      case i if Seq("lw", "lh", "lb", "lhu", "lbu").contains(i) => UOPWB.mem
      case i if Seq("jal", "jalr").contains(i) => UOPWB.pc4
      case _ => UOPWB.dontCare
    }

    override def uopType: UOPWB.type = UOPWB
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
