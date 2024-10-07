// SPDX-License-Identifier: Unlicense
// SPDX-FileCopyrightText: 2024 Jiuyang Liu <liu@jiuyang.me>

package org.chipsalliance.hia

import chisel3._
import chisel3.experimental.hierarchy.{instantiable, public, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.probe.{define, Probe, ProbeValue}
import chisel3.properties.{AnyClassType, Class, Property}
import chisel3.util.{DecoupledIO, Valid}

object HIAParameter {
  implicit def rwP: upickle.default.ReadWriter[HIAParameter] =
    upickle.default.macroRW
}

/** Parameter of [[HIA]] */
case class HIAParameter(width: Int, useAsyncReset: Boolean) extends SerializableModuleParameter {
  val coreParameter = CoreParameter(width)
  val cacheParameter = CacheParameter(width)
}

/** Verification IO of [[HIA]] */
class HIAProbe(parameter: HIAParameter) extends Bundle {
  val busy = Bool()
}

/** Metadata of [[HIA]]. */
@instantiable
class HIAOM(parameter: HIAParameter) extends Class {
  val width:         Property[Int] = IO(Output(Property[Int]()))
  val useAsyncReset: Property[Boolean] = IO(Output(Property[Boolean]()))
  width := Property(parameter.width)
  useAsyncReset := Property(parameter.useAsyncReset)
}

/** Interface of [[HIA]]. */
class HIAInterface(parameter: HIAParameter) extends Bundle {
  val clock = Input(Clock())
  val reset = Input(if (parameter.useAsyncReset) AsyncReset() else Bool())
  val input = Flipped(DecoupledIO(new Bundle {
    val x = UInt(parameter.width.W)
    val y = UInt(parameter.width.W)
  }))
  val output = Valid(UInt(parameter.width.W))
  val probe = Output(Probe(new HIAProbe(parameter), layers.Verification))
  val om = Output(Property[AnyClassType]())
}

/** Hardware Implementation of HIA */
@instantiable
class HIA(val parameter: HIAParameter)
    extends FixedIORawModule(new HIAInterface(parameter))
    with SerializableModule[HIAParameter]
    with ImplicitClock
    with ImplicitReset {
  override protected def implicitClock: Clock = io.clock
  override protected def implicitReset: Reset = io.reset

  val core:  Instance[Core] = Instantiate(new Core(parameter.coreParameter))
  val cache: Instance[Cache] = Instantiate(new Cache(parameter.cacheParameter))
  core.io.icache <> cache.io.icache
  core.io.dcache <> cache.io.dcache

  core.io.clock := io.clock
  core.io.reset := io.reset
  cache.io.clock := io.clock
  cache.io.reset := io.reset

  val x: UInt = Reg(chiselTypeOf(io.input.bits.x))
  // Block X-state propagation
  val y: UInt = RegInit(chiselTypeOf(io.input.bits.x), 0.U)
  val startupFlag = RegInit(false.B)
  val busy = y =/= 0.U

  when(x > y) { x := x - y }.otherwise { y := y - x }

  when(io.input.fire) {
    x := io.input.bits.x
    y := io.input.bits.y
    startupFlag := true.B
  }

  io.input.ready := !busy
  io.output.bits := x
  io.output.valid := startupFlag && !busy

  // Assign Probe
  val probeWire: HIAProbe = Wire(new HIAProbe(parameter))
  define(io.probe, ProbeValue(probeWire))
  probeWire.busy := busy

  // Assign Metadata
  val omInstance: Instance[HIAOM] = Instantiate(new HIAOM(parameter))
  io.om := omInstance.getPropertyReference.asAnyClassType
}
