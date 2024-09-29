package org.chipsalliance.hia

import chisel3._
import chisel3.util._
import chisel3.experimental.hierarchy.{instantiable, public, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import org.chipsalliance.hia.Instructions._

object DecodeStageParameter {
  implicit def rwP: upickle.default.ReadWriter[DecodeStageParameter] = upickle.default.macroRW[DecodeStageParameter]
}

case class DecodeStageParameter() extends SerializableModuleParameter {}

class DecodeStageInterface(parameter: DecodeStageParameter) extends Bundle {}

@instantiable
class DecodeStage(parameter: DecodeStageParameter)
    extends FixedIORawModule(new DecodeStageInterface(parameter))
    with SerializableModule[DecodeStageParameter]
    with Public {

  val WORD_LEN = 32
  val ADDR_LEN = 5 // rs1,rs2,wb
  val CSR_ADDR_LEN = 12
  val BUBBLE = 0x00000013.U(32.W)
  val EXE_FUN_LEN = 5
  val ALU_NONE = 0.U(EXE_FUN_LEN.W)
  val ALU_ADD = 1.U(EXE_FUN_LEN.W)
  val ALU_SUB = 2.U(EXE_FUN_LEN.W)
  val ALU_AND = 3.U(EXE_FUN_LEN.W)
  val ALU_OR = 4.U(EXE_FUN_LEN.W)
  val ALU_XOR = 5.U(EXE_FUN_LEN.W)
  val ALU_SLL = 6.U(EXE_FUN_LEN.W)
  val ALU_SRL = 7.U(EXE_FUN_LEN.W)
  val ALU_SRA = 8.U(EXE_FUN_LEN.W)
  val ALU_SLT = 9.U(EXE_FUN_LEN.W)
  val ALU_SLTU = 10.U(EXE_FUN_LEN.W)
  val BR_BEQ = 11.U(EXE_FUN_LEN.W)
  val BR_BNE = 12.U(EXE_FUN_LEN.W)
  val BR_BLT = 13.U(EXE_FUN_LEN.W)
  val BR_BGE = 14.U(EXE_FUN_LEN.W)
  val BR_BLTU = 15.U(EXE_FUN_LEN.W)
  val BR_BGEU = 16.U(EXE_FUN_LEN.W)
  val ALU_JALR = 17.U(EXE_FUN_LEN.W)
  val ALU_RS1 = 18.U(EXE_FUN_LEN.W) // Copy RS1
  val ALU_VADDVV = 19.U(EXE_FUN_LEN.W)
  val VSET = 20.U(EXE_FUN_LEN.W)
  val ALU_PCNT = 21.U(EXE_FUN_LEN.W)
  val OP1_LEN = 2
  val OP1_RS1 = 0.U(OP1_LEN.W)
  val OP1_PC = 1.U(OP1_LEN.W)
  val OP1_NONE = 2.U(OP1_LEN.W)
  val OP1_IMZ = 3.U(OP1_LEN.W)
  val OP2_LEN = 3
  val OP2_NONE = 0.U(OP2_LEN.W)
  val OP2_RS2 = 1.U(OP2_LEN.W)
  val OP2_IMI = 2.U(OP2_LEN.W)
  val OP2_IMS = 3.U(OP2_LEN.W)
  val OP2_IMJ = 4.U(OP2_LEN.W)
  val OP2_IMU = 5.U(OP2_LEN.W)
  val MEN_LEN = 2
  val MEN_NONE = 0.U(MEN_LEN.W)
  val MEN_SCALAR = 1.U(MEN_LEN.W) // Scalar
  val REN_LEN = 2
  val REN_NONE = 0.U(REN_LEN.W)
  val REN_SCALAR = 1.U(REN_LEN.W) // Scalar
  val CSR_LEN = 3
  val CSR_NONE = 0.U(CSR_LEN.W)
  val CSR_W = 1.U(CSR_LEN.W) // Write
  val CSR_S = 2.U(CSR_LEN.W) // Set bits
  val CSR_C = 3.U(CSR_LEN.W) // Clear bits
  val CSR_E = 4.U(CSR_LEN.W) // Exception (ECALL)
  val CSR_V = 5.U(CSR_LEN.W)

  val inst = Wire(UInt(WORD_LEN.W))
  // ID/EX pipeline reg
  val reg_pc = RegInit(0.U(WORD_LEN.W))
  val reg_wb_addr = RegInit(0.U(ADDR_LEN.W))
  val reg_op1_data = RegInit(0.U(WORD_LEN.W))
  val reg_op2_data = RegInit(0.U(WORD_LEN.W))
  val reg_rs1_data = RegInit(0.U(WORD_LEN.W))
  val reg_rs2_data = RegInit(0.U(WORD_LEN.W))
  val reg_exe_fun = RegInit(0.U(EXE_FUN_LEN.W))
  val reg_mem_wen = RegInit(0.U(MEN_LEN.W)) // memory write enable
  val reg_rf_wen = RegInit(0.U(REN_LEN.W)) // register file write enable
  val reg_wb_sel = RegInit(0.U(WB_SEL_LEN.W))
  // TODO CSR support
  //   val reg_csr_addr = RegInit(0.U(CSR_ADDR_LEN.W))
  //   val reg_csr_cmd = RegInit(0.U(CSR_LEN.W))
  val reg_imm_i_sext = RegInit(0.U(WORD_LEN.W))
  val reg_imm_s_sext = RegInit(0.U(WORD_LEN.W))
  val reg_imm_b_sext = RegInit(0.U(WORD_LEN.W))
  val reg_imm_u_shifted = RegInit(0.U(WORD_LEN.W))
  // val reg_imm_z_uext = RegInit(0.U(WORD_LEN.W))

  def connectStallFlag(prev: FetchStage) = {
    val rs1_addr = prev.reg_inst(19, 15)
    val rs2_addr = prev.reg_inst(24, 20)

    prev.stall_flag := (
      reg_rf_wen === REN_SCALAR &&
        rs1_addr =/= 0.U &&
        rs1_addr === reg_wb_addr
    ) || (
      reg_rf_wen === REN_SCALAR &&
        rs2_addr =/= 0.U &&
        rs2_addr === reg_wb_addr
    )
  }

  def connect(prev: FetchStage, exe: ExecuteStage, mem: Mem, gr: Mem[UInt]) = {
    connectStallFlag(prev)

    // Replace the instruction being fetched with NOP when hazard
    inst := Mux(exe.br_flag || exe.jmp_flag || prev.stall_flag, BUBBLE, prev.reg_inst)
    val rs1_addr = prev.reg_inst(19, 15)
    val rs2_addr = prev.reg_inst(24, 20)
    val wb_addr = prev.reg_inst(11, 7)

    val rs1_data = MuxCase(
      gr(rs1_addr),
      Seq(
        (rs1_addr === 0.U) -> 0.U(WORD_LEN.W)
        // TODO connect forward  output to this stage
      )
    )

    val rs2_data = MuxCase(
      gr(rs2_addr),
      Seq(
        (rs2_addr === 0.U) -> 0.U(WORD_LEN.W)
        // TODO connect forward  output to this stage
      )
    )

    // Decode imm of I-type instruction
    val imm_i = inst(31, 20)
    val imm_i_sext = Cat(Fill(20, imm_i(11)), imm_i)
    // Decode imm of S-type instruction
    val imm_s = Cat(inst(31, 25), inst(11, 6))
    val imm_s_sext = Cat(Fill(20, imm_s(11)), imm_s)
    // imm[0] does not exist in B-type instruction. This is because the first bit of program counter
    // is always zero (p.126). Size of instruction is 32bit or 16bit, so instruction pointer (pc)
    // address always points an even address.
    val imm_b = Cat(inst(31), inst(7), inst(30, 25), inst(11, 8))
    val imm_b_sext = Cat(Fill(19, imm_b(11)), imm_b, 0.U(1.U))
    // Decode imm of J-type instruction
    val imm_j = Cat(inst(31), inst(19, 12), inst(20), inst(30, 21))
    val imm_j_sext = Cat(Fill(11, imm_j(19)), imm_j, 0.U(1.U)) // Set LSB to zero
    // Decode imm of U-type instruction
    val imm_u = inst(31, 12)
    val imm_u_shifted = Cat(imm_u, Fill(12, 0.U)) // for LUI and AUIPC
    // TODO Decode imm of csr instruction
    // val imm_z = inst(19, 15)
    // val imm_z_uext = Cat(Fill(27, 0.U), imm_z) // for CSR instructions

    // FIXME fix exe_fun and other signal
    val List(exe_fun, op1_sel, op2_sel, mem_wen, rf_wen, wb_sel, csr_cmd) = ListLookup(
      inst,
      List(ALU_NONE, OP1_RS1, OP2_RS2, MEN_NONE, REN_NONE, WB_NONE, CSR_NONE),
      Array(
        // RV32I
        LUI -> List(ALU_ADD, OP1_NONE, OP2_IMU, MEN_NONE, REN_SCALAR, WB_ALU, CSR_NONE), // sext(imm_u[31:12] << 12)
        AUIPC -> List(
          ALU_ADD,
          OP1_PC,
          OP2_IMU,
          MEN_NONE,
          REN_SCALAR,
          WB_ALU,
          CSR_NONE
        ), // PC + sext(imm_u[31:12] << 12)
        JAL -> List(
          ALU_ADD,
          OP1_PC,
          OP2_IMJ,
          MEN_NONE,
          REN_SCALAR,
          WB_PC,
          CSR_NONE
        ), // x[rd] <- PC+4 and PC+sext(imm_j)
        JALR -> List(
          ALU_JALR,
          OP1_RS1,
          OP2_IMI,
          MEN_NONE,
          REN_SCALAR,
          WB_PC,
          CSR_NONE
        ), // x[rd] <- PC+4 and (x[rs1]+sext(imm_i))&~1
        BEQ -> List(
          BR_BEQ,
          OP1_RS1,
          OP2_RS2,
          MEN_NONE,
          REN_NONE,
          WB_NONE,
          CSR_NONE
        ), // x[rs1] === x[rs2] then PC+sext(imm_b)
        BNE -> List(
          BR_BNE,
          OP1_RS1,
          OP2_RS2,
          MEN_NONE,
          REN_NONE,
          WB_NONE,
          CSR_NONE
        ), // x[rs1] =/= x[rs2] then PC+sext(imm_b)
        BLT -> List(
          BR_BLT,
          OP1_RS1,
          OP2_RS2,
          MEN_NONE,
          REN_NONE,
          WB_NONE,
          CSR_NONE
        ), // x[rs1] <s x[rs2]  then PC+sext(imm_b)
        BGE -> List(
          BR_BGE,
          OP1_RS1,
          OP2_RS2,
          MEN_NONE,
          REN_NONE,
          WB_NONE,
          CSR_NONE
        ), // x[rs1] >=s x[rs2] then PC+sext(imm_b)
        BLTU -> List(
          BR_BLTU,
          OP1_RS1,
          OP2_RS2,
          MEN_NONE,
          REN_NONE,
          WB_NONE,
          CSR_NONE
        ), // x[rs1] <u x[rs2]  then PC+sext(imm_b)
        BGEU -> List(
          BR_BGEU,
          OP1_RS1,
          OP2_RS2,
          MEN_NONE,
          REN_NONE,
          WB_NONE,
          CSR_NONE
        ), // x[rs1] >=u x[rs2] then PC+sext(imm_b)
        LB -> List(ALU_ADD, OP1_RS1, OP2_IMI, MEN_NONE, REN_SCALAR, WB_MEM, CSR_NONE), // x[rs1] + sext(imm_i)
        LH -> List(ALU_ADD, OP1_RS1, OP2_IMI, MEN_NONE, REN_SCALAR, WB_MEM, CSR_NONE), // x[rs1] + sext(imm_i)
        LW -> List(ALU_ADD, OP1_RS1, OP2_IMI, MEN_NONE, REN_SCALAR, WB_MEM, CSR_NONE), // x[rs1] + sext(imm_i)
        LBU -> List(ALU_ADD, OP1_RS1, OP2_IMI, MEN_NONE, REN_SCALAR, WB_MEM, CSR_NONE), // x[rs1] + sext(imm_i)
        LHU -> List(ALU_ADD, OP1_RS1, OP2_IMI, MEN_NONE, REN_SCALAR, WB_MEM, CSR_NONE), // x[rs1] + sext(imm_i)
        SB -> List(ALU_ADD, OP1_RS1, OP2_IMS, MEN_SCALAR, REN_NONE, WB_NONE, CSR_NONE), // x[rs1] + sext(imm_s)
        SH -> List(ALU_ADD, OP1_RS1, OP2_IMS, MEN_SCALAR, REN_NONE, WB_NONE, CSR_NONE), // x[rs1] + sext(imm_s)
        SW -> List(ALU_ADD, OP1_RS1, OP2_IMS, MEN_SCALAR, REN_NONE, WB_NONE, CSR_NONE), // x[rs1] + sext(imm_s)
        ADDI -> List(ALU_ADD, OP1_RS1, OP2_IMI, MEN_NONE, REN_SCALAR, WB_ALU, CSR_NONE), // x[rs1] + sext(imm_i)
        SLTI -> List(ALU_SLT, OP1_RS1, OP2_IMI, MEN_NONE, REN_SCALAR, WB_ALU, CSR_NONE), // x[rs1] <s imm_i_sext
        SLTIU -> List(ALU_SLTU, OP1_RS1, OP2_IMI, MEN_NONE, REN_SCALAR, WB_ALU, CSR_NONE), // x[rs1] <u imm_i_sext
        XORI -> List(ALU_XOR, OP1_RS1, OP2_IMI, MEN_NONE, REN_SCALAR, WB_ALU, CSR_NONE), // x[rs1] ^ sext(imm_i)
        ORI -> List(ALU_OR, OP1_RS1, OP2_IMI, MEN_NONE, REN_SCALAR, WB_ALU, CSR_NONE), // x[rs1] | sext(imm_i)
        ANDI -> List(ALU_AND, OP1_RS1, OP2_IMI, MEN_NONE, REN_SCALAR, WB_ALU, CSR_NONE), // x[rs1] & sext(imm_i)
        SLLI -> List(ALU_SLL, OP1_RS1, OP2_IMI, MEN_NONE, REN_SCALAR, WB_ALU, CSR_NONE), // x[rs1] << imm_i_sext(4,0)
        SRLI -> List(ALU_SRL, OP1_RS1, OP2_IMI, MEN_NONE, REN_SCALAR, WB_ALU, CSR_NONE), // x[rs1] >>u imm_i_sext(4,0)
        SRAI -> List(ALU_SRA, OP1_RS1, OP2_IMI, MEN_NONE, REN_SCALAR, WB_ALU, CSR_NONE), // x[rs1] >>s imm_i_sext(4,0)
        ADD -> List(ALU_ADD, OP1_RS1, OP2_RS2, MEN_NONE, REN_SCALAR, WB_ALU, CSR_NONE), // x[rs1] + x[rs2]
        SUB -> List(ALU_SUB, OP1_RS1, OP2_RS2, MEN_NONE, REN_SCALAR, WB_ALU, CSR_NONE), // x[rs1] - x[rs2]
        SLL -> List(ALU_SLL, OP1_RS1, OP2_RS2, MEN_NONE, REN_SCALAR, WB_ALU, CSR_NONE), // x[rs1] << x[rs2](4,0)
        SLT -> List(ALU_SLT, OP1_RS1, OP2_RS2, MEN_NONE, REN_SCALAR, WB_ALU, CSR_NONE), // x[rs1] <s x[rs2]
        SLTU -> List(ALU_SLTU, OP1_RS1, OP2_RS2, MEN_NONE, REN_SCALAR, WB_ALU, CSR_NONE), // x[rs1] <u x[rs2]
        XOR -> List(ALU_XOR, OP1_RS1, OP2_RS2, MEN_NONE, REN_SCALAR, WB_ALU, CSR_NONE), // x[rs1] ^ x[rs2]
        SRL -> List(ALU_SRL, OP1_RS1, OP2_RS2, MEN_NONE, REN_SCALAR, WB_ALU, CSR_NONE), // x[rs1] >>u x[rs2](4,0)
        SRA -> List(ALU_SRA, OP1_RS1, OP2_RS2, MEN_NONE, REN_SCALAR, WB_ALU, CSR_NONE), // x[rs1] >>s x[rs2](4,0)
        OR -> List(ALU_OR, OP1_RS1, OP2_RS2, MEN_NONE, REN_SCALAR, WB_ALU, CSR_NONE), // x[rs1] | x[rs2]
        AND -> List(ALU_AND, OP1_RS1, OP2_RS2, MEN_NONE, REN_SCALAR, WB_ALU, CSR_NONE), // x[rs1] & x[rs2]
        FENCE -> List(ALU_NONE, OP1_NONE, OP2_NONE, MEN_NONE, REN_NONE, WB_NONE, CSR_NONE),
        FENCEI -> List(ALU_NONE, OP1_NONE, OP2_NONE, MEN_NONE, REN_NONE, WB_NONE, CSR_NONE),
        ECALL -> List(ALU_NONE, OP1_NONE, OP2_NONE, MEN_NONE, REN_NONE, WB_NONE, CSR_E),
        EBREAK -> List(ALU_NONE, OP1_NONE, OP2_NONE, MEN_NONE, REN_NONE, WB_NONE, CSR_E),
        CSRRW -> List(ALU_RS1, OP1_RS1, OP2_NONE, MEN_NONE, REN_SCALAR, WB_CSR, CSR_W), // CSRs[csr] <- x[rs1]
        CSRRS -> List(
          ALU_RS1,
          OP1_RS1,
          OP2_NONE,
          MEN_NONE,
          REN_SCALAR,
          WB_CSR,
          CSR_S
        ), // CSRs[csr] <- CSRs[csr] | x[rs1]
        CSRRC -> List(
          ALU_RS1,
          OP1_RS1,
          OP2_NONE,
          MEN_NONE,
          REN_SCALAR,
          WB_CSR,
          CSR_C
        ), // CSRs[csr] <- CSRs[csr]&~x[rs1]
        CSRRWI -> List(ALU_RS1, OP1_IMZ, OP2_NONE, MEN_NONE, REN_SCALAR, WB_CSR, CSR_W), // CSRs[csr] <- uext(imm_z)
        CSRRSI -> List(
          ALU_RS1,
          OP1_IMZ,
          OP2_NONE,
          MEN_NONE,
          REN_SCALAR,
          WB_CSR,
          CSR_S
        ), // CSRs[csr] <- CSRs[csr] | uext(imm_z)
        CSRRCI -> List(
          ALU_RS1,
          OP1_IMZ,
          OP2_NONE,
          MEN_NONE,
          REN_SCALAR,
          WB_CSR,
          CSR_C
        ) // CSRs[csr] <- CSRs[csr]&~uext(imm_z)
        // 2.8 Environment Call and Breakpoints
      )
    )

    // TODO csr support
    val op1_data = MuxCase(
      0.U(WORD_LEN.W),
      Seq(
        (op1_sel === OP1_RS1) -> rs1_data,
        (op1_sel === OP1_PC) -> prev.reg_pc
      )
    )

    val op2_data = MuxCase(
      0.U(WORD_LEN.W),
      Seq(
        (op2_sel === OP2_RS2) -> rs2_data,
        (op2_sel === OP2_IMI) -> imm_i_sext,
        (op2_sel === OP2_IMS) -> imm_s_sext,
        (op2_sel === OP2_IMJ) -> imm_j_sext,
        (op2_sel === OP2_IMU) -> imm_u_shifted
      )
    )

    reg_pc := prev.reg_pc
    reg_op1_data := op1_data
    reg_op2_data := op2_data
    reg_rs1_data := rs1_data
    reg_rs2_data := rs2_data
    reg_wb_addr := wb_addr
    reg_rf_wen := rf_wen
    reg_exe_fun := exe_fun
    reg_wb_sel := wb_sel
    reg_imm_i_sext := imm_i_sext
    reg_imm_s_sext := imm_s_sext
    reg_imm_b_sext := imm_b_sext
    reg_imm_u_shifted := imm_u_shifted
    // TODO csr support
    // reg_imm_z_uext    := imm_z_uext
    // reg_csr_addr      := csr_addr
    // reg_csr_cmd       := csr_cmd
    reg_mem_wen := mem_wen
  }
}
