// SPDX-License-Identifier: Unlicense
// SPDX-FileCopyrightText: 2024 Jiuyang Liu <liu@jiuyang.me>

package org.chipsalliance.hia

import chisel3._
import chisel3.experimental.hierarchy.{instantiable, public, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.ltl.Property.{eventually, not}
import chisel3.ltl.{AssertProperty, CoverProperty, Delay, Sequence}
import chisel3.properties.{AnyClassType, Class, Property}
import chisel3.util.circt.dpi.{RawClockedNonVoidFunctionCall, RawUnclockedNonVoidFunctionCall}
import chisel3.util.{Counter, HasExtModuleInline, RegEnable, Valid}
import chisel3.layers.Verification.Assume
import chisel3.ltl.AssumeProperty

object HIAFormalParameter {
  implicit def rwP: upickle.default.ReadWriter[HIAFormalParameter] =
    upickle.default.macroRW
}

/** Parameter of [[HIA]]. */
case class HIAFormalParameter(hiaParameter: HIAParameter) extends SerializableModuleParameter {}

@instantiable
class HIAFormalOM(parameter: HIAFormalParameter) extends Class {
  val hia = IO(Output(Property[AnyClassType]()))
  @public
  val hiaIn = IO(Input(Property[AnyClassType]()))
  hia := hiaIn
}

class HIAFormalInterface(parameter: HIAFormalParameter) extends Bundle {
  val clock = Input(Clock())
  val reset = Input(if (parameter.hiaParameter.useAsyncReset) AsyncReset() else Bool())
  val imem = new instructionFetchAXI(parameter.hiaParameter.width)
  val dmem = new loadStoreAXI(parameter.hiaParameter.width)
  val om = Output(Property[AnyClassType]())
}

@instantiable
class HIAFormal(val parameter: HIAFormalParameter)
    extends FixedIORawModule(new HIAFormalInterface(parameter))
    with SerializableModule[HIAFormalParameter]
    with ImplicitClock
    with ImplicitReset {
  layer.enable(layers.Verification)
  override protected def implicitClock: Clock = io.clock
  override protected def implicitReset: Reset = io.reset
  // Instantiate DUT.
  val dut: Instance[HIA] = Instantiate(new HIA(parameter.hiaParameter))
  // Instantiate OM
  val omInstance = Instantiate(new HIAFormalOM(parameter))
  io.om := omInstance.getPropertyReference.asAnyClassType
  omInstance.hiaIn := dut.io.om

  dut.io.clock := implicitClock
  dut.io.reset := implicitReset

  dut.io.imem <> io.imem
  dut.io.dmem <> io.dmem

  // LTL Checker
  import Sequence._

  val imem_req: Sequence = !io.reset.asBool && io.imem.r.req.valid
  val imem_resp: Sequence = !io.reset.asBool && io.imem.r.resp.valid

  AssumeProperty(imem_req, label = Some("imem_r_valid"))

  AssertProperty(
    imem_req |-> imem_resp,
    label = Some("instruction_fetch_request_response"),
  )
}
