package org.chipsalliance.hia

import chisel3._
import chisel3.util._
import chisel3.experimental.hierarchy.{instantiable, public, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}


class ICacheIO(xlen: Int) extends Bundle {
    // TODO add valid and ready signal
    val addr = Input(UInt(xlen.W))
    val data = Output(UInt(xlen.W))
    val valid = Output(Bool())
    // val mask = UInt((xlen / 8).W)
}

class DCacheIO(xlen: Int) extends  Bundle {
    // TODO add valid and ready signal
    val addr = Input(UInt(xlen.W))
    val data = Output(UInt(xlen.W))
    val wen = Input(Bool())
    val wdata = Input(UInt(xlen.W))
    val valid = Output(Bool())
    val mask = Input(UInt((xlen / 8).W))
}

object CacheParameter {
  implicit def rwP: upickle.default.ReadWriter[CacheParameter] =
    upickle.default.macroRW[CacheParameter]
}

case class CacheParameter(xlen: Int) extends SerializableModuleParameter {}

class CacheInterface(parameter: CacheParameter) extends Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val icache = new ICacheIO(parameter.xlen)
    val dcache = new DCacheIO(parameter.xlen)
}

@instantiable
class Cache(val parameter: CacheParameter)
    extends FixedIORawModule(new CacheInterface(parameter))
    with SerializableModule[CacheParameter]
    with Public 
    with ImplicitClock
    with ImplicitReset {
  override protected def implicitClock: Clock = io.clock
  override protected def implicitReset: Reset = io.reset

  val xlen = parameter.xlen

  val mem = Mem(0x4000, UInt(8.W))

  io.icache.valid := true.B
  io.dcache.valid := true.B

  io.icache.data := Cat(
    mem(io.icache.addr + 3.U(xlen.W)),
    mem(io.icache.addr + 2.U(xlen.W)),
    mem(io.icache.addr + 1.U(xlen.W)),
    mem(io.icache.addr)
  )

  io.dcache.data := Cat(
    mem(io.dcache.addr + 3.U(xlen.W)),
    mem(io.dcache.addr + 2.U(xlen.W)),
    mem(io.dcache.addr + 1.U(xlen.W)),
    mem(io.dcache.addr)
  )

  when(io.dcache.wen) {
    mem(io.dcache.addr + 3.U(xlen.W)) := io.dcache.wdata(31, 24)
    mem(io.dcache.addr + 2.U(xlen.W)) := io.dcache.wdata(23, 16)
    mem(io.dcache.addr + 1.U(xlen.W)) := io.dcache.wdata(15, 8)
    mem(io.dcache.addr) := io.dcache.wdata(7, 0)
  }
}