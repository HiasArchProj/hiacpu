// SPDX-License-Identifier: Unlicense
// SPDX-FileCopyrightText: 2024 Jiuyang Liu <liu@jiuyang.me>
package org.chipsalliance.hia.elaborator

import mainargs._
import org.chipsalliance.hia.{HIA, HIAParameter}
import chisel3.experimental.util.SerializableModuleElaborator

object HIAMain extends SerializableModuleElaborator {
  val topName = "HIA"

  implicit object PathRead extends TokensReader.Simple[os.Path] {
    def shortName = "path"
    def read(strs: Seq[String]) = Right(os.Path(strs.head, os.pwd))
  }

  @main
  case class HIAParameterMain(
    @arg(name = "width") width:                 Int,
    @arg(name = "useAsyncReset") useAsyncReset: Boolean) {
    require(width > 0, "width must be a non-negative integer")
    require(chisel3.util.isPow2(width), "width must be a power of 2")
    def convert: HIAParameter = HIAParameter(width, useAsyncReset)
  }

  implicit def HIAParameterMainParser: ParserForClass[HIAParameterMain] =
    ParserForClass[HIAParameterMain]

  @main
  def config(
    @arg(name = "parameter") parameter:  HIAParameterMain,
    @arg(name = "target-dir") targetDir: os.Path = os.pwd
  ) =
    os.write.over(targetDir / s"${topName}.json", configImpl(parameter.convert))

  @main
  def design(
    @arg(name = "parameter") parameter:  os.Path,
    @arg(name = "target-dir") targetDir: os.Path = os.pwd
  ) = {
    val (firrtl, annos) = designImpl[HIA, HIAParameter](os.read.stream(parameter))
    os.write.over(targetDir / s"${topName}.fir", firrtl)
    os.write.over(targetDir / s"${topName}.anno.json", annos)
  }

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}
