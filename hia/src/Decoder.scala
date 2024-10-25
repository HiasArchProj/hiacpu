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
  // imm_sel
  val IMM_SEL_LEN = 3
  val IMM_S = 0
  val IMM_SB = 1
  val IMM_U = 2
  val IMM_UJ = 3
  val IMM_I = 4
  val IMM_Z = 5

  // pc_sel
  val PC_SEL_LEN = 2
  val PC_4 = 0
  val PC_ALU = 1
  val PC_EPC = 2

  // selAlu1
  val ALU1_SEL_LEN = 2
  val ALU1_PC = 0
  val ALU1_RS1 = 1
  val ALU1_ZERO = 2

  // selAlu2
  val ALU2_SEL_LEN = 2
  val ALU2_IMM = 0
  val ALU2_RS2 = 1
  val ALU2_ZERO = 2

  // br_type
  val BR_TYPE_LEN = 3
  val BR_NONE = 0
  val BR_LTU = 1
  val BR_LT = 2
  val BR_EQ = 3
  val BR_GEU = 4
  val BR_GE = 5
  val BR_NE = 6

  // st_type
  val ST_TYPE_LEN = 2
  val ST_NONE = 0
  val ST_SW = 1
  val ST_SH = 2
  val ST_SB = 3

  // ld_type
  val LD_TYPE_LEN = 3
  val LD_NONE = 0
  val LD_LW = 1
  val LD_LH = 2
  val LD_LB = 3
  val LD_LHU = 4
  val LD_LBU = 5

  // wb_sel
  val WB_SEL_LEN = 2
  val WB_NONE = 0
  val WB_ALU = 1
  val WB_MEM = 2
  val WB_PC4 = 3

  val ALUFN_LEN = 4
  val ALU_ADD = 0
  val ALU_SUB = 1
  val ALU_AND = 2
  val ALU_OR = 3
  val ALU_XOR = 4
  val ALU_SLT = 5
  val ALU_SLL = 6
  val ALU_SLTU = 7
  val ALU_SRL = 8
  val ALU_SRA = 9

  private val instructionTable = org.chipsalliance.rvdecoderdb.instructions(org.chipsalliance.rvdecoderdb.extractResource(getClass.getClassLoader))
  private val targetSets = Set("rv_i", "rv32_i") // TODO add more instructionSets
  private val instructionDecodePattern = instructionTable
    .filter(instr => targetSets.contains(instr.instructionSet.name))
    .filter(_.pseudoFrom.isEmpty)
    .map(HiaDecodePattern(_))
    .toSeq

  private val instructionDecodeFields = Seq(selPC, immType, brType, selAlu1, selAlu2, aluFn, stType, ldType, selWB)

  val table: DecodeTable[HiaDecodePattern] = new DecodeTable[HiaDecodePattern](
    instructionDecodePattern,
    instructionDecodeFields
  )

  object UOPPC extends UOP {
    def width = PC_SEL_LEN

    def pc4: BitPat = encode(PC_4)

    def alu: BitPat = encode(PC_ALU)

    def epc: BitPat = encode(PC_EPC)

  }

  object selPC extends UOPDecodeField[HiaDecodePattern] {
    override def name: String = "pc_sel"

    override def genTable(op: HiaDecodePattern): BitPat = op.instruction.name match {
      case i if Seq("jal", "jalr").contains(i) => UOPPC.alu
      case i if Seq("mret").contains(i) => UOPPC.epc
      case _ => UOPPC.pc4
    }

    override def uopType: UOPPC.type = UOPPC
  }  

  object UOPIMM extends UOP {
    def width = IMM_SEL_LEN

    def s: BitPat = encode(IMM_S)

    def sb: BitPat = encode(IMM_SB)

    def u: BitPat = encode(IMM_U)

    def uj: BitPat = encode(IMM_UJ)

    def i: BitPat = encode(IMM_I)

    def z: BitPat = encode(IMM_Z)
  }

  object immType extends UOPDecodeField[HiaDecodePattern] {
    override def name: String = "imm_type"

    override def genTable(op: HiaDecodePattern): BitPat = op.instruction.name match {
      // format: off
      case i if Seq("ori", "lhu", "lw", "andi", "sltiu", "lh", "jalr", "lbu", "xori", "slti", "addi", "lb", "srli", "srai", "slli", "ld", "sraiw", "lwu", "addiw", "srliw", "slliw").contains(i) => UOPIMM.i
      case i if Seq("sb", "sd", "sh", "sw").contains(i) => UOPIMM.s
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
    def width = BR_TYPE_LEN

    def none: BitPat = encode(BR_NONE)

    def ltu: BitPat = encode(BR_LTU)

    def lt: BitPat = encode(BR_LT)

    def eq: BitPat = encode(BR_EQ)

    def geu: BitPat = encode(BR_GEU)

    def ge: BitPat = encode(BR_GE)

    def ne: BitPat = encode(BR_NE)
  }

  object brType extends UOPDecodeField[HiaDecodePattern] {
    override def name: String = "br_type"

    override def genTable(op: HiaDecodePattern): BitPat = op.instruction.name match {
      case "bltu" => UOPBR.ltu
      case "blt"  => UOPBR.lt
      case "beq"  => UOPBR.eq
      case "bgeu" => UOPBR.geu
      case "bge"  => UOPBR.ge
      case "bne"  => UOPBR.ne
      case _      => UOPBR.none
    }

    override def uopType: UOPBR.type = UOPBR
  }

  object UOPA1 extends UOP {
    def width = ALU1_SEL_LEN

    def pc: BitPat = encode(ALU1_PC)

    def rs1: BitPat = encode(ALU1_RS1)

    def zero: BitPat = encode(ALU1_ZERO)
  }

  object selAlu1 extends UOPDecodeField[HiaDecodePattern] {
    override def name: String = "sel_alu1"

    override def genTable(op: HiaDecodePattern): BitPat = op.instruction.name match {
      case i if Seq("auipc", "jal", "beq", "bne", "blt", "bge", "bltu", "bgeu").contains(i) => UOPA1.pc
      case i
          if Seq(
            "jalr",
            "lb",
            "lh",
            "lw",
            "lbu",
            "lhu",
            "sb",
            "sh",
            "sw",
            "addi",
            "slti",
            "sltiu",
            "xori",
            "ori",
            "andi",
            "add",
            "sub",
            "sll",
            "slt",
            "sltu",
            "xor",
            "srl",
            "sra",
            "or",
            "and"
          ).contains(i) =>
        UOPA1.rs1
      case i if Seq("lui").contains(i) => UOPA1.zero
      case _                           => UOPA1.dontCare
    }
    override def uopType: UOPA1.type = UOPA1
  }

  object UOPA2 extends UOP {
    def width = ALU2_SEL_LEN

    def imm: BitPat = encode(ALU2_IMM)

    def rs2: BitPat = encode(ALU2_RS2)

    def zero: BitPat = encode(ALU2_ZERO)
  }
  object selAlu2 extends UOPDecodeField[HiaDecodePattern] {
    override def name: String = "sel_alu2"

    override def genTable(op: HiaDecodePattern): BitPat = op.instruction.name match {
      case i
          if Seq(
            "lui",
            "auipc",
            "jalr",
            "jal",
            "beq",
            "bne",
            "blt",
            "bge",
            "bltu",
            "bgeu",
            "lb",
            "lh",
            "lw",
            "lbu",
            "lhu",
            "sb",
            "sh",
            "sw",
            "addi",
            "slti",
            "sltiu",
            "xori",
            "ori",
            "andi"
          ).contains(i) =>
        UOPA2.imm
      case i if Seq("add", "sub", "sll", "slt", "sltu", "xor", "srl", "sra", "or", "and").contains(i) => UOPA2.rs2
      case _                                                                                          => UOPA2.dontCare
    }
    override def uopType: UOPA2.type = UOPA2
  }

  object UOPALU extends UOP {
    def width = ALUFN_LEN

    def add:  BitPat = encode(ALU_ADD)
    def sub:  BitPat = encode(ALU_SUB)
    def and:  BitPat = encode(ALU_AND)
    def or:   BitPat = encode(ALU_OR)
    def xor:  BitPat = encode(ALU_XOR)
    def slt:  BitPat = encode(ALU_SLT)
    def sll:  BitPat = encode(ALU_SLL)
    def sltu: BitPat = encode(ALU_SLTU)
    def srl:  BitPat = encode(ALU_SRL)
    def sra:  BitPat = encode(ALU_SRA)
  }

  object aluFn extends UOPDecodeField[HiaDecodePattern] {
    override def name: String = "aluFn"

    override def genTable(op: HiaDecodePattern): BitPat = op.instruction.name match {
      case i
          if Seq("lui", "auipc", "jalr", "jal", "beq", "bne", "blt", "bge", "bltu", "bgeu", "lb", "lh", "lw", "lbu", "lhu", "sb", "sh", "sw", "addi", "add")
            .contains(i) =>
        UOPALU.add
      case i if Seq("sub").contains(i)           => UOPALU.sub
      case i if Seq("andi", "and").contains(i)   => UOPALU.and
      case i if Seq("ori", "or").contains(i)     => UOPALU.or
      case i if Seq("xori", "xor").contains(i)   => UOPALU.xor
      case i if Seq("slti", "slt").contains(i)   => UOPALU.slt
      case i if Seq("slli", "sll").contains(i)   => UOPALU.sll
      case i if Seq("sltiu", "sltu").contains(i) => UOPALU.sltu
      case i if Seq("srli", "srl").contains(i)   => UOPALU.srl
      case i if Seq("srai", "sra").contains(i)   => UOPALU.sra
      case _                                     => UOPALU.dontCare
    }

    override def uopType: UOPALU.type = UOPALU
  }

  object UOPST extends UOP {
    def width = ST_TYPE_LEN

    def sw: BitPat = encode(ST_SW)

    def sh: BitPat = encode(ST_SH)

    def sb: BitPat = encode(ST_SB)

    def none: BitPat = encode(ST_NONE) // be used to get wen
  }

  object stType extends UOPDecodeField[HiaDecodePattern] {
    override def name: String = "st_type"

    override def genTable(op: HiaDecodePattern): BitPat = op.instruction.name match {
      case i if Seq("sw").contains(i) => UOPST.sw
      case i if Seq("sh").contains(i) => UOPST.sh
      case i if Seq("sb").contains(i) => UOPST.sb
      case _                          => UOPST.none
    }

    override def uopType: UOPST.type = UOPST
  }

  object UOPLD extends UOP {
    def width = LD_TYPE_LEN

    def none: BitPat = encode(LD_NONE)

    def lw: BitPat = encode(LD_LW)

    def lh: BitPat = encode(LD_LH)

    def lb: BitPat = encode(LD_LB)

    def lhu: BitPat = encode(LD_LHU)

    def lbu: BitPat = encode(LD_LBU)
  }
  object ldType extends UOPDecodeField[HiaDecodePattern] {
    override def name: String = "ld_type"

    override def genTable(op: HiaDecodePattern): BitPat = op.instruction.name match {
      case i if Seq("lw").contains(i)  => UOPLD.lw
      case i if Seq("lh").contains(i)  => UOPLD.lh
      case i if Seq("lb").contains(i)  => UOPLD.lb
      case i if Seq("lhu").contains(i) => UOPLD.lhu
      case i if Seq("lbu").contains(i) => UOPLD.lbu
      case _                           => UOPLD.none
    }

    override def uopType: UOPLD.type = UOPLD
  }

  object UOPWB extends UOP {
    def width = WB_SEL_LEN

    def alu: BitPat = encode(WB_ALU)

    def mem: BitPat = encode(WB_MEM)

    def pc4: BitPat = encode(WB_PC4)

    def none: BitPat = encode(WB_NONE) // be used to get wb_en
  }

  object selWB extends UOPDecodeField[HiaDecodePattern] {
    override def name: String = "sel_wb"

    override def genTable(op: HiaDecodePattern): BitPat = op.instruction.name match {
      case i
          if Seq(
            "lui",
            "auipc",
            "beq",
            "bne",
            "blt",
            "bge",
            "bltu",
            "bgeu",
            "sb",
            "sh",
            "sw",
            "addi",
            "slti",
            "sltiu",
            "xori",
            "ori",
            "andi",
            "add",
            "sub",
            "sll",
            "slt",
            "sltu",
            "xor",
            "srl",
            "sra",
            "or",
            "and"
          ).contains(i) =>
        UOPWB.alu
      case i if Seq("lw", "lh", "lb", "lhu", "lbu").contains(i) => UOPWB.mem
      case i if Seq("jal", "jalr").contains(i)                  => UOPWB.pc4
      case _                                                    => UOPWB.none
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
