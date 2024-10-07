package org.chipsalliance.hia

import chisel3._
import chisel3.util._
import chisel3.experimental.hierarchy.{instantiable, public, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}

// import org.chipsalliance.hia.Cache.{DCacheIO, ICacheIO}

object CoreParameter {
  implicit def rwP: upickle.default.ReadWriter[CoreParameter] =
    upickle.default.macroRW[CoreParameter]
}

case class CoreParameter(xlen: Int) extends SerializableModuleParameter {
    val ctrlparameter = ControlParameter()
    val datapathParameter = DatapathParameter(xlen, ctrlparameter)
}

class CoreInterface(parameter: CoreParameter) extends Bundle {
    val icache = Flipped(new ICacheIO(parameter.xlen))
    val dcache = Flipped(new DCacheIO(parameter.xlen))
}

@instantiable
class Core(val parameter: CoreParameter)
    extends FixedIORawModule(new CoreInterface(parameter))
    with SerializableModule[CoreParameter]
    with Public {
    
    val dpath = Instantiate(new Datapath(parameter.datapathParameter))
    val ctrl = Instantiate(new Control(parameter.ctrlparameter))
    io.dcache <> dpath.io.dcache
    io.icache <> dpath.io.icache
    ctrl.io <> dpath.io.ctrl
}