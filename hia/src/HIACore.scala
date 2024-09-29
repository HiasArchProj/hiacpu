package org.chipsalliance.hia

import chisel3._
import chisel3.util._
import chisel3.experimental.hierarchy.{instantiable, public, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}


class ImemPortIO(width: Int)  extends Bundle {
    val addr = Input(UInt(width.W))
    val inst = Input(UInt(width.W))
}

class DmemPortIO(width: Int) extends Bundle {
    val addr = Input(UInt(width.W))
    val rdata = Input(UInt(width.W))
    val wen = Input(Bool())
    val wdata = Input(UInt(width.W))
}

object HIACoreParameter {
  implicit def rwP: upickle.default.ReadWriter[HIACoreParameter] = upickle.default.macroRW
}

case class HIACoreParameter(width: Int) extends SerializableModuleParameter {}

class HIACoreInterface(parameter: HIACoreParameter) extends Bundle  {
    val imem = Flipped(new ImemPortIO(parameter.width))
    val dmem = Flipped(new DmemPortIO(parameter.width))

    val exit = Output(Bool()) // for riscv-tests
    val gp = Output(UInt(WORD_LEN.W)) // for riscv-tests
    val pc = Output(UInt(WORD_LEN.W)) // for riscv-tests  
}

@instantiable
class HIACore(val parameter: HIACoreParameter) 
    extends FixedIORawModule(new HIACoreInterface(parameter))
    with SerializableModule[HIACoreParameter]
    with Public 
{
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