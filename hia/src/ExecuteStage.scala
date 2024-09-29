package org.chipsalliance.hia

import chisel3._
import chisel3.util._
import chisel3.experimental.hierarchy.{instantiable, public, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}

object ExecuteStageParameter {
  implicit def rwP: upickle.default.ReadWriter[ExecuteStageParameter] = upickle.default.macroRW[ExecuteStageParameter]
}

case class ExecuteStageParameter() extends SerializableModuleParameter {}

class ExecuteStageInterface(parameter: ExecuteStageParameter) extends Bundle {}

@instantiable
class ExecuteStage(parameter: ExecuteStageParameter)
    extends FixedIORawModule(new ExecuteStageInterface(parameter))
    with SerializableModule[ExecuteStageParameter]
    with Public {

  val REN_LEN = 2
  val WORD_LEN = 32
  val ADDR_LEN = 5 // rs1,rs2,wb
  val MEN_LEN = 2
  val CSR_LEN = 3
  val CSR_ADDR_LEN = 12
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
  val VSET = 20.U(EXE_FUN_LEN.W) // TODO Remove vset
  val ALU_PCNT = 21.U(EXE_FUN_LEN.W)
  val WB_SEL_LEN = 3
  val WB_NONE = 0.U(wb_sel_len.W)
  val WB_ALU = 0.U(wb_sel_len.W)
  val WB_MEM = 1.U(wb_sel_len.W)
  val WB_PC = 2.U(wb_sel_len.W)
  val WB_CSR = 3.U(wb_sel_len.W)
  val WB_MEM_V = 4.U(wb_sel_len.W)
  val WB_ALU_V = 5.U(wb_sel_len.W)
  val WB_VL = 6.U(wb_sel_len.W)
  val br_flag = Wire(Bool())
  val br_target = Wire(UInt(WORD_LEN.W))
  val jmp_flag = Wire(Bool())
  val alu_out = Wire(UInt(WORD_LEN.W))

  // EX/MEM pipline reg
  val reg_pc = RegInit(0.U(WORD_LEN.W))
  val reg_wb_addr = RegInit(0.U(ADDR_LEN.W))
  val reg_op1_data = RegInit(0.U(WORD_LEN.W))
  val reg_rs1_data = RegInit(0.U(WORD_LEN.W))
  val reg_rs2_data = RegInit(0.U(WORD_LEN.W))
  val reg_mem_wen = RegInit(0.U(MEN_LEN.W))
  val reg_rf_wen = RegInit(0.U(REN_LEN.W))
  val reg_wb_sel = RegInit(0.U(WB_SEL_LEN.W))
  val reg_csr_addr = RegInit(0.U(CSR_ADDR_LEN.W))
  val reg_csr_cmd = RegInit(0.U(CSR_LEN.W))
  val reg_imm_i_sext = RegInit(0.U(WORD_LEN.W))
  val reg_imm_z_uext = RegInit(0.U(WORD_LEN.W))
  val reg_alu_out = RegInit(0.U(WORD_LEN.W))

  def connect(prev: DecodeStage) = {
    // Arithmetic Logic Unit process arithmetic/logical calculations for each instruction.
    alu_out := MuxCase(
      0.U(WORD_LEN.W),
      Seq(
        (prev.reg_exe_fun === ALU_ADD) -> (prev.reg_op1_data + prev.reg_op2_data),
        (prev.reg_exe_fun === ALU_SUB) -> (prev.reg_op1_data - prev.reg_op2_data),
        (prev.reg_exe_fun === ALU_AND) -> (prev.reg_op1_data & prev.reg_op2_data),
        (prev.reg_exe_fun === ALU_OR) -> (prev.reg_op1_data | prev.reg_op2_data),
        (prev.reg_exe_fun === ALU_XOR) -> (prev.reg_op1_data ^ prev.reg_op2_data),
        // Note: (31, 0) is necessary because << extends bits of the result value
        // Note: (4, 0) is necessary for I instructions (imm[4:0])
        (prev.reg_exe_fun === ALU_SLL) -> (prev.reg_op1_data << prev.reg_op2_data(4, 0))(31, 0),
        (prev.reg_exe_fun === ALU_SRL) -> (prev.reg_op1_data >> prev.reg_op2_data(4, 0)).asUInt,
        (prev.reg_exe_fun === ALU_SRA) -> (prev.reg_op1_data.asSInt >> prev.reg_op2_data(4, 0)).asUInt,
        // Compare as signed integers
        (prev.reg_exe_fun === ALU_SLT) -> (prev.reg_op1_data.asSInt < prev.reg_op2_data.asSInt).asUInt,
        (prev.reg_exe_fun === ALU_SLTU) -> (prev.reg_op1_data < prev.reg_op2_data).asUInt,
        // &~1 sets the LSB to zero (& 0b1111..1110) for jump instructions
        (prev.reg_exe_fun === ALU_JALR) -> ((prev.reg_op1_data + prev.reg_op2_data) & ~1.U(WORD_LEN.W)),
        (prev.reg_exe_fun === ALU_RS1) -> prev.reg_op1_data
      )
    )

    // Branch instructions
    br_flag := MuxCase(
      false.B,
      Seq(
        (prev.reg_exe_fun === BR_BEQ) -> (prev.reg_op1_data === prev.reg_op2_data),
        (prev.reg_exe_fun === BR_BNE) -> !(prev.reg_op1_data === prev.reg_op2_data),
        (prev.reg_exe_fun === BR_BLT) -> (prev.reg_op1_data.asSInt < prev.reg_op2_data.asSInt),
        (prev.reg_exe_fun === BR_BGE) -> !(prev.reg_op1_data.asSInt < prev.reg_op2_data.asSInt),
        (prev.reg_exe_fun === BR_BLTU) -> (prev.reg_op1_data < prev.reg_op2_data),
        (prev.reg_exe_fun === BR_BGEU) -> !(prev.reg_op1_data < prev.reg_op2_data)
      )
    )
    br_target := prev.reg_pc + prev.reg_imm_b_sext

    jmp_flag := prev.reg_wb_sel === WB_PC

    // Save EX states for next stage
    reg_pc := prev.reg_pc
    reg_op1_data := prev.reg_op1_data
    reg_rs1_data := prev.reg_rs1_data
    reg_rs2_data := prev.reg_rs2_data
    reg_wb_addr := prev.reg_wb_addr
    reg_alu_out := alu_out
    reg_rf_wen := prev.reg_rf_wen
    reg_wb_sel := prev.reg_wb_sel
    // reg_csr_addr   := prev.reg_csr_addr
    // reg_csr_cmd    := prev.reg_csr_cmd
    reg_imm_i_sext := prev.reg_imm_i_sext
    // reg_imm_z_uext := prev.reg_imm_z_uext
    reg_mem_wen := prev.reg_mem_wen
  }
}
