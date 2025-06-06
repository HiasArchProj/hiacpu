package org.chipsalliance.hia

import chisel3.util.BitPat

object Instructions {
  // Loads
  def LB = BitPat("b?????????????????000?????0000011")
  def LH = BitPat("b?????????????????001?????0000011")
  def LW = BitPat("b?????????????????010?????0000011")
  def LBU = BitPat("b?????????????????100?????0000011")
  def LHU = BitPat("b?????????????????101?????0000011")
  // Stores
  def SB = BitPat("b?????????????????000?????0100011")
  def SH = BitPat("b?????????????????001?????0100011")
  def SW = BitPat("b?????????????????010?????0100011")
  // Shifts
  def SLL = BitPat("b0000000??????????001?????0110011")
  def SLLI = BitPat("b0000000??????????001?????0010011")
  def SRL = BitPat("b0000000??????????101?????0110011")
  def SRLI = BitPat("b0000000??????????101?????0010011")
  def SRA = BitPat("b0100000??????????101?????0110011")
  def SRAI = BitPat("b0100000??????????101?????0010011")
  // Arithmetic
  def ADD = BitPat("b0000000??????????000?????0110011")
  def ADDI = BitPat("b?????????????????000?????0010011")
  def SUB = BitPat("b0100000??????????000?????0110011")
  def LUI = BitPat("b?????????????????????????0110111")
  def AUIPC = BitPat("b?????????????????????????0010111")
  // Logical
  def XOR = BitPat("b0000000??????????100?????0110011")
  def XORI = BitPat("b?????????????????100?????0010011")
  def OR = BitPat("b0000000??????????110?????0110011")
  def ORI = BitPat("b?????????????????110?????0010011")
  def AND = BitPat("b0000000??????????111?????0110011")
  def ANDI = BitPat("b?????????????????111?????0010011")
  // Compare
  def SLT = BitPat("b0000000??????????010?????0110011")
  def SLTI = BitPat("b?????????????????010?????0010011")
  def SLTU = BitPat("b0000000??????????011?????0110011")
  def SLTIU = BitPat("b?????????????????011?????0010011")
  // Branches
  def BEQ = BitPat("b?????????????????000?????1100011")
  def BNE = BitPat("b?????????????????001?????1100011")
  def BLT = BitPat("b?????????????????100?????1100011")
  def BGE = BitPat("b?????????????????101?????1100011")
  def BLTU = BitPat("b?????????????????110?????1100011")
  def BGEU = BitPat("b?????????????????111?????1100011")
  // Jump & Link
  def JAL = BitPat("b?????????????????????????1101111")
  def JALR = BitPat("b?????????????????000?????1100111")
  // Synch
  def FENCE = BitPat("b0000????????00000000000000001111")
  def FENCEI = BitPat("b00000000000000000001000000001111")
  // CSR Access
  def CSRRW = BitPat("b?????????????????001?????1110011")
  def CSRRS = BitPat("b?????????????????010?????1110011")
  def CSRRC = BitPat("b?????????????????011?????1110011")
  def CSRRWI = BitPat("b?????????????????101?????1110011")
  def CSRRSI = BitPat("b?????????????????110?????1110011")
  def CSRRCI = BitPat("b?????????????????111?????1110011")
  // Change Level
  def ECALL = BitPat("b00000000000000000000000001110011")
  def EBREAK = BitPat("b00000000000100000000000001110011")
  def ERET = BitPat("b00010000000000000000000001110011")
  def WFI = BitPat("b00010000001000000000000001110011")

  def NOP = BitPat.bitPatToUInt(BitPat("b00000000000000000000000000010011"))
}
