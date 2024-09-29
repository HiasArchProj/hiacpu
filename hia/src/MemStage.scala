package org.chipsalliance.hia

import chisel3._
import chisel3.util._
import chisel3.experimental.hierarchy.{instantiable, public, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}

object MemStageParameter {
    implicit def rwP: upickle.default.ReadWriter[MemStageParameter] = upickle.default.macroRW[MemStageParameter]
}

case class MemStageParameter() extends SerializableModuleParameter {}

class MemStageInterface(parameter: MemStageParameter) extends Bundle {}

@instantiable
class MemStage(parameter: MemStageParameter) 
    extends FixedIORawModule(new MemStageInterface(parameter))
    with SerializableModule[MemStageParameter]
    with Public {

  val REN_LEN    = 2
  val WORD_LEN      = 32
  val ADDR_LEN      = 5 // rs1,rs2,wb
  val WB_SEL_LEN = 3
  val WB_NONE    = 0.U(WB_SEL_LEN.W)
  val WB_ALU     = 0.U(WB_SEL_LEN.W)
  val WB_MEM     = 1.U(WB_SEL_LEN.W)
  val WB_PC      = 2.U(WB_SEL_LEN.W)
  val WB_CSR     = 3.U(WB_SEL_LEN.W)
  val WB_MEM_V   = 4.U(WB_SEL_LEN.W)
  val WB_ALU_V   = 5.U(WB_SEL_LEN.W)
  val WB_VL      = 6.U(WB_SEL_LEN.W)

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