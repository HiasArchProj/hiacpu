// SPDX-License-Identifier: Unlicense
// SPDX-FileCopyrightText: 2024 Jiuyang Liu <liu@jiuyang.me>
package org.chipsalliance.hia.elaborator

import mainargs._
import org.chipsalliance.hia.{HIATestBench, HIATestBenchParameter, TestVerbatimParameter}
import org.chipsalliance.hia.elaborator.Elaborator
import org.chipsalliance.hia.elaborator.HIA.HIAParameterMain

object HIATestBench extends Elaborator {
  @main
  case class HIATestBenchParameterMain(
    @arg(name = "testVerbatimParameter") testVerbatimParameter: TestVerbatimParameterMain,
    @arg(name = "hiaParameter") hiaParameter:                   HIAParameterMain,
    @arg(name = "timeout") timeout:                             Int,
    @arg(name = "testSize") testSize:                           Int) {
    def convert: HIATestBenchParameter = HIATestBenchParameter(
      testVerbatimParameter.convert,
      hiaParameter.convert,
      timeout,
      testSize
    )
  }

  case class TestVerbatimParameterMain(
    @arg(name = "useAsyncReset") useAsyncReset:       Boolean,
    @arg(name = "initFunctionName") initFunctionName: String,
    @arg(name = "dumpFunctionName") dumpFunctionName: String,
    @arg(name = "clockFlipTick") clockFlipTick:       Int,
    @arg(name = "resetFlipTick") resetFlipTick:       Int) {
    def convert: TestVerbatimParameter = TestVerbatimParameter(
      useAsyncReset:    Boolean,
      initFunctionName: String,
      dumpFunctionName: String,
      clockFlipTick:    Int,
      resetFlipTick:    Int
    )
  }

  implicit def TestVerbatimParameterMainParser: ParserForClass[TestVerbatimParameterMain] =
    ParserForClass[TestVerbatimParameterMain]

  implicit def HIAParameterMainParser: ParserForClass[HIAParameterMain] =
    ParserForClass[HIAParameterMain]

  implicit def HIATestBenchParameterMainParser: ParserForClass[HIATestBenchParameterMain] =
    ParserForClass[HIATestBenchParameterMain]

  @main
  def config(@arg(name = "parameter") parameter: HIATestBenchParameterMain) =
    configImpl(parameter.convert)

  @main
  def design(
    @arg(name = "parameter") parameter:    os.Path,
    @arg(name = "run-firtool") runFirtool: mainargs.Flag,
    @arg(name = "target-dir") targetDir:   os.Path
  ) =
    designImpl[HIATestBench, HIATestBenchParameter](parameter, runFirtool.value, targetDir)

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}
