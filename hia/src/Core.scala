package org.chipsalliance.hia

import chisel3._
import chisel3.util._
import Consts._
import Instructions._


class FetchStage {
  val stall_flag = Wire(Bool()) // True when data hazard occurs at EX stage
  // IF/ID pipeline reg
  val reg_pc = RegInit(0.U(WORD_LEN.W))
  val reg_inst = RegInit(0.U(WORD_LEN.W))

  def connect(imem: ImemPortIO, exe: ExecuteStage, csr: Mem[UInt]) = {
    val pc = RegInit(START_ADDR)
    pc := MuxCase(
      pc + 4.U(WORD_LEN.W),
      Seq(
        exe.br_flag -> exe.br_target,
        exe.jmp_flag -> exe.alu_out,
        stall_flag -> pc // TODO add syscall
      )
    )
    imem.addr := pc
    reg_pc := Mux(stall_flag, reg_pc, pc)

    val inst = imem.inst
    reg_inst := MuxCase(
      inst,
      Seq(
        (exe.br_flag || exe.jmp_flag) -> BUBBLE, // Prioritize branch hazard over data hazard
        stall_flag -> reg_inst
      )
    )
  }
}

class DecodeStage {
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

    val rs1_data  = MuxCase(gr(rs1_addr), Seq(
      (rs1_addr === 0.U) -> 0.U(WORD_LEN.W),
      // TODO connect forward  output to this stage
    ))

    val rs2_data  = MuxCase(gr(rs2_addr), Seq(
      (rs2_addr === 0.U) -> 0.U(WORD_LEN.W),
      // TODO connect forward  output to this stage
    ))    

    // Decode imm of I-type instruction
    val imm_i = inst(31, 20)
    val imm_i_sext = Cat(Fill(20, imm_i(11)), imm_i) 
    // Decode imm of S-type instruction
    val imm_s = Cat(inst(31,25), inst(11,6)) 
    val imm_s_sext = Cat(Fill(20, imm_s(11)), imm_s) 
    // imm[0] does not exist in B-type instruction. This is because the first bit of program counter
    // is always zero (p.126). Size of instruction is 32bit or 16bit, so instruction pointer (pc)
    // address always points an even address.
    val imm_b = Cat(inst(31), inst(7), inst(30,25), inst(11,8))
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
        LUI     -> List(ALU_ADD,  OP1_NONE, OP2_IMU,  MEN_NONE,   REN_SCALAR, WB_ALU,   CSR_NONE), // sext(imm_u[31:12] << 12)
        AUIPC   -> List(ALU_ADD,  OP1_PC,   OP2_IMU,  MEN_NONE,   REN_SCALAR, WB_ALU,   CSR_NONE), // PC + sext(imm_u[31:12] << 12)
        JAL     -> List(ALU_ADD,  OP1_PC,   OP2_IMJ,  MEN_NONE,   REN_SCALAR, WB_PC,    CSR_NONE), // x[rd] <- PC+4 and PC+sext(imm_j)
        JALR    -> List(ALU_JALR, OP1_RS1,  OP2_IMI,  MEN_NONE,   REN_SCALAR, WB_PC,    CSR_NONE), // x[rd] <- PC+4 and (x[rs1]+sext(imm_i))&~1
        BEQ     -> List(BR_BEQ,   OP1_RS1,  OP2_RS2,  MEN_NONE,   REN_NONE,   WB_NONE,  CSR_NONE), // x[rs1] === x[rs2] then PC+sext(imm_b)
        BNE     -> List(BR_BNE,   OP1_RS1,  OP2_RS2,  MEN_NONE,   REN_NONE,   WB_NONE,  CSR_NONE), // x[rs1] =/= x[rs2] then PC+sext(imm_b)
        BLT     -> List(BR_BLT,   OP1_RS1,  OP2_RS2,  MEN_NONE,   REN_NONE,   WB_NONE,  CSR_NONE), // x[rs1] <s x[rs2]  then PC+sext(imm_b)
        BGE     -> List(BR_BGE,   OP1_RS1,  OP2_RS2,  MEN_NONE,   REN_NONE,   WB_NONE,  CSR_NONE), // x[rs1] >=s x[rs2] then PC+sext(imm_b)
        BLTU    -> List(BR_BLTU,  OP1_RS1,  OP2_RS2,  MEN_NONE,   REN_NONE,   WB_NONE,  CSR_NONE), // x[rs1] <u x[rs2]  then PC+sext(imm_b)
        BGEU    -> List(BR_BGEU,  OP1_RS1,  OP2_RS2,  MEN_NONE,   REN_NONE,   WB_NONE,  CSR_NONE), // x[rs1] >=u x[rs2] then PC+sext(imm_b)
        LB      -> List(ALU_ADD,  OP1_RS1,  OP2_IMI,  MEN_NONE,   REN_SCALAR, WB_MEM,   CSR_NONE), // x[rs1] + sext(imm_i)
        LH      -> List(ALU_ADD,  OP1_RS1,  OP2_IMI,  MEN_NONE,   REN_SCALAR, WB_MEM,   CSR_NONE), // x[rs1] + sext(imm_i)
        LW      -> List(ALU_ADD,  OP1_RS1,  OP2_IMI,  MEN_NONE,   REN_SCALAR, WB_MEM,   CSR_NONE), // x[rs1] + sext(imm_i)
        LBU      -> List(ALU_ADD,  OP1_RS1,  OP2_IMI,  MEN_NONE,   REN_SCALAR, WB_MEM,   CSR_NONE), // x[rs1] + sext(imm_i)
        LHU      -> List(ALU_ADD,  OP1_RS1,  OP2_IMI,  MEN_NONE,   REN_SCALAR, WB_MEM,   CSR_NONE), // x[rs1] + sext(imm_i)
        SB      -> List(ALU_ADD,  OP1_RS1,  OP2_IMS,  MEN_SCALAR, REN_NONE,   WB_NONE,  CSR_NONE), // x[rs1] + sext(imm_s)
        SH      -> List(ALU_ADD,  OP1_RS1,  OP2_IMS,  MEN_SCALAR, REN_NONE,   WB_NONE,  CSR_NONE), // x[rs1] + sext(imm_s)
        SW      -> List(ALU_ADD,  OP1_RS1,  OP2_IMS,  MEN_SCALAR, REN_NONE,   WB_NONE,  CSR_NONE), // x[rs1] + sext(imm_s)
        ADDI    -> List(ALU_ADD,  OP1_RS1,  OP2_IMI,  MEN_NONE,   REN_SCALAR, WB_ALU,   CSR_NONE), // x[rs1] + sext(imm_i)
        SLTI    -> List(ALU_SLT,  OP1_RS1,  OP2_IMI,  MEN_NONE,   REN_SCALAR, WB_ALU,   CSR_NONE), // x[rs1] <s imm_i_sext
        SLTIU   -> List(ALU_SLTU, OP1_RS1,  OP2_IMI,  MEN_NONE,   REN_SCALAR, WB_ALU,   CSR_NONE), // x[rs1] <u imm_i_sext
        XORI    -> List(ALU_XOR,  OP1_RS1,  OP2_IMI,  MEN_NONE,   REN_SCALAR, WB_ALU,   CSR_NONE), // x[rs1] ^ sext(imm_i)
        ORI     -> List(ALU_OR ,  OP1_RS1,  OP2_IMI,  MEN_NONE,   REN_SCALAR, WB_ALU,   CSR_NONE), // x[rs1] | sext(imm_i)
        ANDI    -> List(ALU_AND,  OP1_RS1,  OP2_IMI,  MEN_NONE,   REN_SCALAR, WB_ALU,   CSR_NONE), // x[rs1] & sext(imm_i)
        SLLI    -> List(ALU_SLL,  OP1_RS1,  OP2_IMI,  MEN_NONE,   REN_SCALAR, WB_ALU,   CSR_NONE), // x[rs1] << imm_i_sext(4,0)
        SRLI    -> List(ALU_SRL,  OP1_RS1,  OP2_IMI,  MEN_NONE,   REN_SCALAR, WB_ALU,   CSR_NONE), // x[rs1] >>u imm_i_sext(4,0)
        SRAI    -> List(ALU_SRA,  OP1_RS1,  OP2_IMI,  MEN_NONE,   REN_SCALAR, WB_ALU,   CSR_NONE), // x[rs1] >>s imm_i_sext(4,0)
        ADD     -> List(ALU_ADD,  OP1_RS1,  OP2_RS2,  MEN_NONE,   REN_SCALAR, WB_ALU,   CSR_NONE), // x[rs1] + x[rs2]
        SUB     -> List(ALU_SUB,  OP1_RS1,  OP2_RS2,  MEN_NONE,   REN_SCALAR, WB_ALU,   CSR_NONE), // x[rs1] - x[rs2]
        SLL     -> List(ALU_SLL,  OP1_RS1,  OP2_RS2,  MEN_NONE,   REN_SCALAR, WB_ALU,   CSR_NONE), // x[rs1] << x[rs2](4,0)
        SLT     -> List(ALU_SLT,  OP1_RS1,  OP2_RS2,  MEN_NONE,   REN_SCALAR, WB_ALU,   CSR_NONE), // x[rs1] <s x[rs2]
        SLTU    -> List(ALU_SLTU, OP1_RS1,  OP2_RS2,  MEN_NONE,   REN_SCALAR, WB_ALU,   CSR_NONE), // x[rs1] <u x[rs2]
        XOR     -> List(ALU_XOR,  OP1_RS1,  OP2_RS2,  MEN_NONE,   REN_SCALAR, WB_ALU,   CSR_NONE), // x[rs1] ^ x[rs2]
        SRL     -> List(ALU_SRL,  OP1_RS1,  OP2_RS2,  MEN_NONE,   REN_SCALAR, WB_ALU,   CSR_NONE), // x[rs1] >>u x[rs2](4,0)
        SRA     -> List(ALU_SRA,  OP1_RS1,  OP2_RS2,  MEN_NONE,   REN_SCALAR, WB_ALU,   CSR_NONE), // x[rs1] >>s x[rs2](4,0)
        OR      -> List(ALU_OR,   OP1_RS1,  OP2_RS2,  MEN_NONE,   REN_SCALAR, WB_ALU,   CSR_NONE), // x[rs1] | x[rs2]
        AND     -> List(ALU_AND,  OP1_RS1,  OP2_RS2,  MEN_NONE,   REN_SCALAR, WB_ALU,   CSR_NONE), // x[rs1] & x[rs2]
        FENCE   -> List(ALU_NONE, OP1_NONE, OP2_NONE, MEN_NONE,   REN_NONE,   WB_NONE,  CSR_NONE),
        FENCEI   -> List(ALU_NONE, OP1_NONE, OP2_NONE, MEN_NONE,   REN_NONE,   WB_NONE,  CSR_NONE),
        ECALL   -> List(ALU_NONE, OP1_NONE, OP2_NONE, MEN_NONE,   REN_NONE,   WB_NONE,  CSR_E),
        EBREAK   -> List(ALU_NONE, OP1_NONE, OP2_NONE, MEN_NONE,   REN_NONE,   WB_NONE,  CSR_E),
        CSRRW   -> List(ALU_RS1,  OP1_RS1,  OP2_NONE, MEN_NONE,   REN_SCALAR, WB_CSR,   CSR_W), // CSRs[csr] <- x[rs1]
        CSRRS   -> List(ALU_RS1,  OP1_RS1,  OP2_NONE, MEN_NONE,   REN_SCALAR, WB_CSR,   CSR_S), // CSRs[csr] <- CSRs[csr] | x[rs1]
        CSRRC   -> List(ALU_RS1,  OP1_RS1,  OP2_NONE, MEN_NONE,   REN_SCALAR, WB_CSR,   CSR_C), // CSRs[csr] <- CSRs[csr]&~x[rs1]
        CSRRWI  -> List(ALU_RS1,  OP1_IMZ,  OP2_NONE, MEN_NONE,   REN_SCALAR, WB_CSR,   CSR_W), // CSRs[csr] <- uext(imm_z)
        CSRRSI  -> List(ALU_RS1,  OP1_IMZ,  OP2_NONE, MEN_NONE,   REN_SCALAR, WB_CSR,   CSR_S), // CSRs[csr] <- CSRs[csr] | uext(imm_z)
        CSRRCI  -> List(ALU_RS1,  OP1_IMZ,  OP2_NONE, MEN_NONE,   REN_SCALAR, WB_CSR,   CSR_C), // CSRs[csr] <- CSRs[csr]&~uext(imm_z)
        // 2.8 Environment Call and Breakpoints
      )
    )

    // TODO csr support
    val op1_data = MuxCase(0.U(WORD_LEN.W), Seq(
      (op1_sel === OP1_RS1) -> rs1_data,
      (op1_sel === OP1_PC) -> prev.reg_pc,
    ))

    val op2_data = MuxCase(0.U(WORD_LEN.W), Seq(
      (op2_sel === OP2_RS2) -> rs2_data,
      (op2_sel === OP2_IMI) -> imm_i_sext,
      (op2_sel === OP2_IMS) -> imm_s_sext,
      (op2_sel === OP2_IMJ) -> imm_j_sext,
      (op2_sel === OP2_IMU) -> imm_u_shifted,
    ))

    reg_pc            := prev.reg_pc
    reg_op1_data      := op1_data
    reg_op2_data      := op2_data
    reg_rs1_data      := rs1_data
    reg_rs2_data      := rs2_data
    reg_wb_addr       := wb_addr
    reg_rf_wen        := rf_wen
    reg_exe_fun       := exe_fun
    reg_wb_sel        := wb_sel
    reg_imm_i_sext    := imm_i_sext
    reg_imm_s_sext    := imm_s_sext
    reg_imm_b_sext    := imm_b_sext
    reg_imm_u_shifted := imm_u_shifted
    // TODO csr support
    // reg_imm_z_uext    := imm_z_uext
    // reg_csr_addr      := csr_addr
    // reg_csr_cmd       := csr_cmd
    reg_mem_wen       := mem_wen
  }
}

class ExecuteStage {
  val br_flag = Wire(Bool())
  val br_target = Wire(UInt(WORD_LEN.W))
  val jmp_flag = Wire(Bool())
  val alu_out = Wire(UInt(WORD_LEN.W))

  // EX/MEM pipline reg
  val reg_pc         = RegInit(0.U(WORD_LEN.W))
  val reg_wb_addr    = RegInit(0.U(ADDR_LEN.W))
  val reg_op1_data   = RegInit(0.U(WORD_LEN.W))
  val reg_rs1_data   = RegInit(0.U(WORD_LEN.W))
  val reg_rs2_data   = RegInit(0.U(WORD_LEN.W))
  val reg_mem_wen    = RegInit(0.U(MEN_LEN.W))
  val reg_rf_wen     = RegInit(0.U(REN_LEN.W))
  val reg_wb_sel     = RegInit(0.U(WB_SEL_LEN.W))
  val reg_csr_addr   = RegInit(0.U(CSR_ADDR_LEN.W))
  val reg_csr_cmd    = RegInit(0.U(CSR_LEN.W))
  val reg_imm_i_sext = RegInit(0.U(WORD_LEN.W))
  val reg_imm_z_uext = RegInit(0.U(WORD_LEN.W))
  val reg_alu_out    = RegInit(0.U(WORD_LEN.W))

  def connect(prev: DecodeStage) = {
        // Arithmetic Logic Unit process arithmetic/logical calculations for each instruction.
    alu_out := MuxCase(0.U(WORD_LEN.W), Seq(
      (prev.reg_exe_fun === ALU_ADD)  -> (prev.reg_op1_data + prev.reg_op2_data),
      (prev.reg_exe_fun === ALU_SUB)  -> (prev.reg_op1_data - prev.reg_op2_data),
      (prev.reg_exe_fun === ALU_AND)  -> (prev.reg_op1_data & prev.reg_op2_data),
      (prev.reg_exe_fun === ALU_OR)   -> (prev.reg_op1_data | prev.reg_op2_data),
      (prev.reg_exe_fun === ALU_XOR)  -> (prev.reg_op1_data ^ prev.reg_op2_data),
      // Note: (31, 0) is necessary because << extends bits of the result value
      // Note: (4, 0) is necessary for I instructions (imm[4:0])
      (prev.reg_exe_fun === ALU_SLL)  -> (prev.reg_op1_data << prev.reg_op2_data(4, 0))(31, 0),
      (prev.reg_exe_fun === ALU_SRL)  -> (prev.reg_op1_data >> prev.reg_op2_data(4, 0)).asUInt,
      (prev.reg_exe_fun === ALU_SRA)  -> (prev.reg_op1_data.asSInt >> prev.reg_op2_data(4, 0)).asUInt,
      // Compare as signed integers
      (prev.reg_exe_fun === ALU_SLT)  -> (prev.reg_op1_data.asSInt < prev.reg_op2_data.asSInt).asUInt,
      (prev.reg_exe_fun === ALU_SLTU) -> (prev.reg_op1_data < prev.reg_op2_data).asUInt,
      // &~1 sets the LSB to zero (& 0b1111..1110) for jump instructions
      (prev.reg_exe_fun === ALU_JALR) -> ((prev.reg_op1_data + prev.reg_op2_data) & ~1.U(WORD_LEN.W)),
      (prev.reg_exe_fun === ALU_RS1) -> prev.reg_op1_data,
    ))

    // Branch instructions
    br_flag := MuxCase(false.B, Seq(
      (prev.reg_exe_fun === BR_BEQ)  ->  (prev.reg_op1_data === prev.reg_op2_data),
      (prev.reg_exe_fun === BR_BNE)  -> !(prev.reg_op1_data === prev.reg_op2_data),
      (prev.reg_exe_fun === BR_BLT)  ->  (prev.reg_op1_data.asSInt < prev.reg_op2_data.asSInt),
      (prev.reg_exe_fun === BR_BGE)  -> !(prev.reg_op1_data.asSInt < prev.reg_op2_data.asSInt),
      (prev.reg_exe_fun === BR_BLTU) ->  (prev.reg_op1_data < prev.reg_op2_data),
      (prev.reg_exe_fun === BR_BGEU) -> !(prev.reg_op1_data < prev.reg_op2_data),
    ))
    br_target := prev.reg_pc + prev.reg_imm_b_sext

    jmp_flag := prev.reg_wb_sel === WB_PC

    // Save EX states for next stage
    reg_pc         := prev.reg_pc
    reg_op1_data   := prev.reg_op1_data
    reg_rs1_data   := prev.reg_rs1_data
    reg_rs2_data   := prev.reg_rs2_data
    reg_wb_addr    := prev.reg_wb_addr
    reg_alu_out    := alu_out
    reg_rf_wen     := prev.reg_rf_wen
    reg_wb_sel     := prev.reg_wb_sel
    // reg_csr_addr   := prev.reg_csr_addr
    // reg_csr_cmd    := prev.reg_csr_cmd
    reg_imm_i_sext := prev.reg_imm_i_sext
    // reg_imm_z_uext := prev.reg_imm_z_uext
    reg_mem_wen    := prev.reg_mem_wen
  }
}

class MemStage {
  val wb_data = Wire(UInt(WORD_LEN.W)) // Declare wire for forwarding 
  // MEM/WB pipeline reg
  val reg_wb_addr = RegInit(0.U(ADDR_LEN.W))
  val reg_rf_wen  = RegInit(0.U(REN_LEN.W))
  val reg_wb_data = RegInit(0.U(WORD_LEN.W))

  // TODO csr support
  def connect(dmem: DmemPortIO, prev: ExecuteStage, decode: DecodeStage) {
    wb_data := MuxCase(prev.reg_alu_out, Seq(
      (prev.reg_wb_sel === WB_MEM) -> dmem.rdata,
      (prev.reg_wb_sel === WB_PC) -> (prev.reg_pc + 4.U(WORD_LEN.W)),
    ))

    reg_wb_addr := prev.reg_wb_addr
    reg_rf_wen := prev.reg_rf_wen
    reg_wb_data := wb_data 
  } 
}

class WriteBackStage {
  def connect(prev: MemStage, gr: Mem[UInt]) {
    // FIXME change REN_SCALAR to REN?
    when(prev.reg_rf_wen === REN_SCALAR) {
      gr(prev.reg_wb_addr) := prev.reg_wb_data
    } 
  }
}

class ImemPortIO extends Bundle {
    val addr = Input(UInt(WORD_LEN.W))
    val inst = Input(UInt(WORD_LEN.W))
}

class DmemPortIO extends Bundle {
    val addr = Input(UInt(WORD_LEN.W))
    val rdata = Input(UInt(WORD_LEN.W))
    val wen = Input(Bool())
    val wdata = Input(UInt(WORD_LEN.W))
}

class Core extends Module {
  val io = IO(new Bundle {
    val imem = Flipped(new ImemPortIO())
    val dmem = Flipped(new DmemPortIO())

    val exit = Output(Bool()) // for riscv-tests
    val gp = Output(UInt(WORD_LEN.W)) // for riscv-tests
    val pc = Output(UInt(WORD_LEN.W)) // for riscv-tests
  })

  val regfile = Mem(32, UInt(WORD_LEN.W))
  val csr_regfile = Mem(4096, UInt(WORD_LEN.W))

  val fetch = new FetchStage()
  val decode = new DecodeStage()
  val execute = new ExecuteStage()
  val mem = new MemStage()
  val wb = new WriteBackStage()

  fetch.connect(io.imem, execute, csr_regfile)
  decode.connect(mem, regfile)
  execute.connect(decode)
  mem.connect(io.dmem, execute, decode)
  wb.connect(mem, regfile)

  // exit when jump addr === pc
  io.exit := execute.jmp_flag && (decode.reg_pc === execute.alu_out)
  io.pc := execute.reg_pc
  io.gp := regfile(3)
}