// SPDX-License-Identifier: Unlicense
// SPDX-FileCopyrightText: 2024 Jiuyang Liu <liu@jiuyang.me>

package org.chipsalliance.hia

import chisel3._
import chisel3.experimental.hierarchy.{instantiable, public, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.ltl.Property.{eventually, not}
import chisel3.ltl.{AssertProperty, CoverProperty, Delay, Sequence}
import chisel3.probe.{define, Probe, ProbeValue}
import chisel3.properties.{AnyClassType, Class, Property}
import chisel3.util.circt.dpi.{RawClockedNonVoidFunctionCall, RawUnclockedNonVoidFunctionCall}
import chisel3.util.{Counter, DecoupledIO, HasExtModuleInline, RegEnable, Valid}

object HIAParameter {
  implicit def rwP: upickle.default.ReadWriter[HIAParameter] =
    upickle.default.macroRW
}

/** Parameter of [[HIA]] */
case class HIAParameter(width: Int, useAsyncReset: Boolean) extends SerializableModuleParameter

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

object HIATestBenchParameter {
  implicit def rwP: upickle.default.ReadWriter[HIATestBenchParameter] =
    upickle.default.macroRW
}

/** Parameter of [[HIA]]. */
case class HIATestBenchParameter(
  testVerbatimParameter: TestVerbatimParameter,
  hiaParameter:          HIAParameter,
  timeout:               Int,
  testSize:              Int)
    extends SerializableModuleParameter {
  require(
    (testVerbatimParameter.useAsyncReset && hiaParameter.useAsyncReset) ||
      (!testVerbatimParameter.useAsyncReset && !hiaParameter.useAsyncReset),
    "Reset Type check failed."
  )
}

@instantiable
class HIATestBenchOM(parameter: HIATestBenchParameter) extends Class {
  val hia = IO(Output(Property[AnyClassType]()))
  @public
  val hiaIn = IO(Input(Property[AnyClassType]()))
  hia := hiaIn
}

class HIATestBenchInterface(parameter: HIATestBenchParameter) extends Bundle {
  val om = Output(Property[AnyClassType]())
}

@instantiable
class HIATestBench(val parameter: HIATestBenchParameter)
    extends FixedIORawModule(new HIATestBenchInterface(parameter))
    with SerializableModule[HIATestBenchParameter]
    with ImplicitClock
    with ImplicitReset {
  override protected def implicitClock: Clock = verbatim.io.clock
  override protected def implicitReset: Reset = verbatim.io.reset
  // Instantiate Drivers
  val verbatim: Instance[TestVerbatim] = Instantiate(
    new TestVerbatim(parameter.testVerbatimParameter)
  )
  // Instantiate DUT.
  val dut: Instance[HIA] = Instantiate(new HIA(parameter.hiaParameter))
  // Instantiate OM
  val omInstance = Instantiate(new HIATestBenchOM(parameter))
  io.om := omInstance.getPropertyReference.asAnyClassType
  omInstance.hiaIn := dut.io.om

  dut.io.clock := implicitClock
  dut.io.reset := implicitReset

  // Simulation Logic
  val simulationTime: UInt = RegInit(0.U(64.W))
  simulationTime := simulationTime + 1.U
  // For each timeout ticks, check it
  val (_, callWatchdog) = Counter(true.B, parameter.timeout / 2)
  val watchdogCode = RawUnclockedNonVoidFunctionCall("hia_watchdog", UInt(8.W))(callWatchdog)
  when(watchdogCode =/= 0.U) {
    stop(cf"""{"event":"SimulationStop","reason": ${watchdogCode},"cycle":${simulationTime}}\n""")
  }
  class TestPayload extends Bundle {
    val x = UInt(parameter.hiaParameter.width.W)
    val y = UInt(parameter.hiaParameter.width.W)
    val result = UInt(parameter.hiaParameter.width.W)
  }
  val request =
    RawClockedNonVoidFunctionCall("hia_input", Valid(new TestPayload))(
      dut.io.clock,
      !dut.io.reset.asBool && dut.io.input.ready
    )
  when(dut.io.input.ready) {
    dut.io.input.valid := request.valid
    dut.io.input.bits := request.bits
  }.otherwise {
    dut.io.input.valid := false.B;
    dut.io.input.bits := DontCare;
  }

  // LTL Checker
  import Sequence._
  val inputFire:         Sequence = dut.io.input.fire
  val inputNotFire:      Sequence = !dut.io.input.fire
  val outputFire:        Sequence = dut.io.output.valid
  val outputNotFire:     Sequence = !dut.io.output.valid
  val lastRequestResult: UInt = RegEnable(request.bits.result, dut.io.input.fire)
  val checkRight:        Sequence = lastRequestResult === dut.io.output.bits
  val inputNotValid:     Sequence = dut.io.input.ready && !dut.io.input.valid

  AssertProperty(
    inputFire |=> inputNotFire.repeatAtLeast(1) ### outputFire,
    label = Some("HIA_ALWAYS_RESPONSE")
  )
  AssertProperty(
    inputFire |=> not(inputNotFire.repeatAtLeast(1) ### (outputNotFire.and(inputFire))),
    label = Some("HIA_NO_DOUBLE_FIRE")
  )
  AssertProperty(
    outputFire |-> checkRight,
    label = Some("HIA_ASSERT_RESULT_CHECK")
  )
  // TODO: need generate $rose function in SVA
  // CoverProperty(
  //   rose(outputFire).nonConsecutiveRepeat(parameter.testSize - 1),
  //   label = Some("HIA_COVER_FIRE")
  // )
  CoverProperty(
    inputNotValid,
    label = Some("HIA_COVER_BACK_PRESSURE")
  )
}
object TestVerbatimParameter {
  implicit def rwP: upickle.default.ReadWriter[TestVerbatimParameter] =
    upickle.default.macroRW
}

case class TestVerbatimParameter(
  useAsyncReset:    Boolean,
  initFunctionName: String,
  dumpFunctionName: String,
  clockFlipTick:    Int,
  resetFlipTick:    Int)
    extends SerializableModuleParameter

@instantiable
class TestVerbatimOM(parameter: TestVerbatimParameter) extends Class {
  val useAsyncReset:    Property[Boolean] = IO(Output(Property[Boolean]()))
  val initFunctionName: Property[String] = IO(Output(Property[String]()))
  val dumpFunctionName: Property[String] = IO(Output(Property[String]()))
  val clockFlipTick:    Property[Int] = IO(Output(Property[Int]()))
  val resetFlipTick:    Property[Int] = IO(Output(Property[Int]()))
  val hia = IO(Output(Property[AnyClassType]()))
  @public
  val hiaIn = IO(Input(Property[AnyClassType]()))
  hia := hiaIn
  useAsyncReset := Property(parameter.useAsyncReset)
  initFunctionName := Property(parameter.initFunctionName)
  dumpFunctionName := Property(parameter.dumpFunctionName)
  clockFlipTick := Property(parameter.clockFlipTick)
  resetFlipTick := Property(parameter.resetFlipTick)
}

/** Test blackbox for clockgen, wave dump and extra testbench-only codes. */
class TestVerbatimInterface(parameter: TestVerbatimParameter) extends Bundle {
  val clock: Clock = Output(Clock())
  val reset: Reset = Output(
    if (parameter.useAsyncReset) AsyncReset() else Bool()
  )
}

@instantiable
class TestVerbatim(parameter: TestVerbatimParameter)
    extends FixedIOExtModule(new TestVerbatimInterface(parameter))
    with HasExtModuleInline {
  setInline(
    s"$desiredName.sv",
    s"""module $desiredName(output reg clock, output reg reset);
       |  export "DPI-C" function ${parameter.dumpFunctionName};
       |  function ${parameter.dumpFunctionName}(input string file);
       |`ifdef VCS
       |    $$fsdbDumpfile(file);
       |    $$fsdbDumpvars("+all");
       |    $$fsdbDumpSVA;
       |    $$fsdbDumpon;
       |`endif
       |`ifdef VERILATOR
       |    $$dumpfile(file);
       |    $$dumpvars(0);
       |`endif
       |  endfunction;
       |
       |  import "DPI-C" context function void ${parameter.initFunctionName}();
       |  initial begin
       |    ${parameter.initFunctionName}();
       |    clock = 1'b0;
       |    reset = 1'b1;
       |  end
       |  initial #(${parameter.resetFlipTick}) reset = 1'b0;
       |  always #${parameter.clockFlipTick} clock = ~clock;
       |endmodule
       |""".stripMargin
  )
}
