package org.chipsalliance.hia

import chisel3._
import chisel3.util._
import chisel3.experimental.hierarchy.{instantiable, public, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}

object CacheParameter {
  implicit def rwP: upickle.default.ReadWriter[CacheParameter] =
    upickle.default.macroRW[CacheParameter]
}

case class CacheParameter(xlen: Int) extends SerializableModuleParameter {}

class CacheInterface(parameter: CacheParameter) extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())
  // to cpu
  val icache = new ICacheIO(parameter.xlen)
  val dcache = new DCacheIO(parameter.xlen)
  // to memory
  val imem = new instructionFetchAXI(parameter.xlen)
  val dmem = new loadStoreAXI(parameter.xlen)

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

  // icache read memory
  io.imem.r.req.addr := io.icache.raddr
  io.icache.rdata := io.imem.r.resp.bits.data
  
  // dcache read memory
  io.dmem.r.req.addr := io.dcache.raddr
  io.dcache.rdata := io.dmem.r.resp.bits.data

  // dcache write memory
  io.dmem.w.req.valid := io.dcache.wen
  io.dmem.w.req.bits.addr := io.dcache.waddr
  io.dmem.w.req.bits.data := io.dcache.wdata
}
