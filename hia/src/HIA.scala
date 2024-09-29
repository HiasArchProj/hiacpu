// SPDX-License-Identifier: Unlicense
// SPDX-FileCopyrightText: 2024 Jiuyang Liu <liu@jiuyang.me>

package org.chipsalliance.hia

import chisel3._
import chisel3.experimental.hierarchy.{instantiable, public, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.probe.{define, Probe, ProbeValue}
import chisel3.properties.{AnyClassType, Class, Property}
import chisel3.util.{DecoupledIO, Valid}

// import org.chipsalliance.hia.{Core, DmemPortIO, ImemPortIO, WORD_LEN}

object HIAParameter {
  implicit def rwP: upickle.default.ReadWriter[HIAParameter] =
    upickle.default.macroRW
}

/** Parameter of [[HIA]] */
case class HIAParameter(width: Int, useAsyncReset: Boolean) extends SerializableModuleParameter {
  val hiacoreParameter: HIACoreParameter = HIACoreParameter(width)
}

/** Verification IO of [[HIA]] */
// class HIAProbe(parameter: HIAParameter) extends Bundle {
//   val busy = Bool()
// }

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
  val WORD_LEN = 32

  val clock = Input(Clock())
  val reset = Input(if (parameter.useAsyncReset) AsyncReset() else Bool())
  val exit = Output(Bool())
  val pc = Output(UInt(WORD_LEN.W))
  val gp = Output(UInt(WORD_LEN.W))
  val imem = Flipped(new ImemPortIO(parameter.width))
  val dmem = Flipped(new DmemPortIO(parameter.width))


  val input = Flipped(DecoupledIO(new Bundle {
    val x = UInt(parameter.width.W)
    val y = UInt(parameter.width.W)
  }))
  val output = Valid(UInt(parameter.width.W))
  // val probe = Output(Probe(new HIAProbe(parameter), layers.Verification))
  val om = Output(Property[AnyClassType]())
}

/** Hardware Implementation of HIA */
@instantiable
class HIA(val parameter: HIAParameter)
    extends FixedIORawModule(new HIAInterface(parameter))
    with SerializableModule[HIAParameter]
    // with ImplicitClock
    // with ImplicitReset 
    {
  // override protected def implicitClock: Clock = io.clock
  // override protected def implicitReset: Reset = io.reset

  val core = Instantiate(new HIACore(parameter.hiacoreParameter))
  core.io.imem <> io.imem
  core.io.dmem <> io.dmem
  core.io.exit := io.exit
  core.io.gp := io.gp
  core.io.pc := io.pc



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

  Assign Probe
  val probeWire: HIAProbe = Wire(new HIAProbe(parameter))
  define(io.probe, ProbeValue(probeWire))
  probeWire.busy := busy

  // Assign Metadata
  val omInstance: Instance[HIAOM] = Instantiate(new HIAOM(parameter))
  io.om := omInstance.getPropertyReference.asAnyClassType
}
