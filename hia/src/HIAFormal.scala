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

  val imem_req:  Sequence = !io.reset.asBool && io.imem.r.req.valid
  val imem_resp: Sequence = !io.reset.asBool && io.imem.r.resp.valid

  AssumeProperty(imem_req, label = Some("imem_r_valid"))

  // Assert that imem request is eventually followed by a response
  AssertProperty(
    imem_req |-> eventually(imem_resp),
    label = Some("imem_req_eventually_resp")
  )

  // Assert that imem response is eventually followed by a request
  AssertProperty(
    io.reset.asBool |-> not(io.imem.r.resp.valid),
    label = Some("imem_resp_no_reset")
  )

  // Data memory request and response properties
  val dmem_req:  Sequence = !io.reset.asBool && io.dmem.r.req.valid
  val dmem_resp: Sequence = !io.reset.asBool && io.dmem.r.resp.valid

  // Assert that dmem request is eventually followed by a response
  AssertProperty(
    dmem_req |-> eventually(dmem_resp),
    label = Some("dmem_req_eventually_resp")
  )

  // Assert that dmem response is eventually followed by a request
  AssertProperty(
    io.reset.asBool |-> not(io.dmem.r.resp.valid),
    label = Some("dmem_resp_no_reset")
  )

  // Cover properties for imem and dmem requests
  val wb_en = io.dmem.w.req.valid
  // Writeback enable is true
  AssertProperty(
    wb_en |-> (io.dmem.w.req.bits.addr =/= 0.U),
    label = Some("wb_en_valid_addr")
  )

  // These should be connected to the corresponding signals in your DUT
  val id_valid = dut.io.probe.id_valid // Instruction decode stage valid
  val ex_valid = dut.io.probe.ex_valid // Execute stage valid
  val id_rs1 = dut.io.probe.id_rs1 // Source register 1 in decode stage
  val id_rs2 = dut.io.probe.id_rs2 // Source register 2 in decode stage
  val id_rs1_en = dut.io.probe.id_rs1_en // Source 1 enable
  val id_rs2_en = dut.io.probe.id_rs2_en // Source 2 enable
  val ex_rd = dut.io.probe.ex_rd // Destination register in execute stage
  val ex_wen = dut.io.probe.ex_wen // Write enable in execute stage
  val id_stall = dut.io.probe.id_stall // Stall signal in decode stage
  val bypass_en = dut.io.probe.bypass_en // Bypass enable signal

  // RAW hazard detection for rs1: if decode stage reads a register that execute stage will write
  val raw_hazard_rs1 = id_valid && ex_valid &&
    id_rs1_en && ex_wen &&
    (id_rs1 === ex_rd) && (ex_rd =/= 0.U)

  // Assert: If a RAW hazard on rs1 occurs, there must be a stall or bypass
  AssertProperty(
    raw_hazard_rs1 |-> (id_stall || bypass_en),
    label = Some("RAW_hazard_rs1_bypass_or_stall")
  )

  // RAW hazard detection for rs2
  val raw_hazard_rs2 = id_valid && ex_valid &&
    id_rs2_en && ex_wen &&
    (id_rs2 === ex_rd) && (ex_rd =/= 0.U)

  // Assert: If a RAW hazard on rs2 occurs, there must be a stall or bypass
  AssertProperty(
    raw_hazard_rs2 |-> (id_stall || bypass_en),
    label = Some("RAW_hazard_rs2_bypass_or_stall")
  )
}
