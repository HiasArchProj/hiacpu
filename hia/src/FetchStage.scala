package org.chipsalliance.hia

import chisel3._
import chisel3.util._
import chisel3.experimental.hierarchy.{instantiable, public, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}

object FetchStageParameter {
  implicit def rwP: upickle.default.ReadWriter[FetchStageParameter] = upickle.default.macroRW[FetchStageParameter]
}

case class FetchStageParameter() extends SerializableModuleParameter {}

class FetchStageInterface(parameter: FetchStageParameter) extends Bundle {}

@instantiable
class FetchStage(parameter: FetchStageParameter)
    extends FixedIORawModule(new FetchStageInterface(parameter))
    with SerializableModule[FetchStageParameter]
    with Public {

  val WORD_LEN = 32
  val START_ADDR = 0.U(WORD_LEN.W)
  val BUBBLE = 0x00000013.U(WORD_LEN.W)

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
