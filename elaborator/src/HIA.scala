// SPDX-License-Identifier: Unlicense
// SPDX-FileCopyrightText: 2024 Jiuyang Liu <liu@jiuyang.me>
package org.chipsalliance.hia.elaborator

import mainargs._
import org.chipsalliance.hia.{HIA, HIAParameter}
import org.chipsalliance.hia.elaborator.Elaborator

object HIA extends Elaborator {
  @main
  case class HIAParameterMain(
    @arg(name = "xLen") xLen:                   Int,
    @arg(name = "useAsyncReset") useAsyncReset: Boolean) {
    def convert: HIAParameter = HIAParameter(xLen, useAsyncReset)
  }

  implicit def HIAParameterMainParser: ParserForClass[HIAParameterMain] =
    ParserForClass[HIAParameterMain]

  @main
  def config(@arg(name = "parameter") parameter: HIAParameterMain) = configImpl(
    parameter.convert
  )

  @main
  def design(
    @arg(name = "parameter") parameter:    os.Path,
    @arg(name = "run-firtool") runFirtool: mainargs.Flag,
    @arg(name = "target-dir") targetDir:   os.Path
  ) =
    designImpl[HIA, HIAParameter](parameter, runFirtool.value, targetDir)

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}
