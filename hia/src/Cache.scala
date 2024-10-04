package org.chipsalliance.hia

import chisel3._
import chisel3.util._
import chisel3.experimental.hierarchy.{instantiable, public, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}


class ICacheIO(xlen: Int) extends Bundle {
    val addr = Input(UInt(xlen.W))
    val data = Output(UInt(xlen.W))
}

class DCacheIO(xlen: Int) extends  Bundle {
    val addr = Input(UInt(xlen.W))
    val data = Output(UInt(xlen.W))
    val wen = Input(Bool())
    val wdata = Input(UInt(xlen.W))
}

object CacheParameter {
  implicit def rwP: upickle.default.ReadWriter[CacheParameter] =
    upickle.default.macroRW[CacheParameter]
}

case class CacheParameter(xlen: Int) extends SerializableModuleParameter {}

class CacheInterface(parameter: CacheParameter) extends Bundle {
    val icache = new ICacheIO(parameter.xlen)
    val dcache = new ICacheIO(parameter.xlen)
}

@instantiable
class Cache(val parameter: CacheParameter)
    extends FixedIORawModule(new CacheInterface(parameter))
    with SerializableModule[CacheParameter]
    with Public {

}