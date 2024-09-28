// SPDX-License-Identifier: Unlicense
// SPDX-FileCopyrightText: 2024 Jiuyang Liu <liu@jiuyang.me>
package org.chipsalliance.hia.elaborator

import mainargs._
import org.chipsalliance.hia.{HIAFormal, HIAFormalParameter}
import org.chipsalliance.hia.elaborator.HIAMain.HIAParameterMain
import chisel3.experimental.util.SerializableModuleElaborator

object HIAFormalMain extends SerializableModuleElaborator {
  val topName = "HIAFormal"

  implicit object PathRead extends TokensReader.Simple[os.Path] {
    def shortName = "path"
    def read(strs: Seq[String]) = Right(os.Path(strs.head, os.pwd))
  }

  @main
  case class HIAFormalParameterMain(
    @arg(name = "hiaParameter") hiaParameter: HIAParameterMain) {
    def convert: HIAFormalParameter = HIAFormalParameter(hiaParameter.convert)
  }

  implicit def HIAParameterMainParser: ParserForClass[HIAParameterMain] =
    ParserForClass[HIAParameterMain]

  implicit def HIAFormalParameterMainParser: ParserForClass[HIAFormalParameterMain] =
    ParserForClass[HIAFormalParameterMain]

  @main
  def config(
    @arg(name = "parameter") parameter:  HIAFormalParameterMain,
    @arg(name = "target-dir") targetDir: os.Path = os.pwd
  ) =
    os.write.over(targetDir / s"${topName}.json", configImpl(parameter.convert))

  @main
  def design(
    @arg(name = "parameter") parameter:  os.Path,
    @arg(name = "target-dir") targetDir: os.Path = os.pwd
  ) = {
    val (firrtl, annos) = designImpl[HIAFormal, HIAFormalParameter](os.read.stream(parameter))
    os.write.over(targetDir / s"${topName}.fir", firrtl)
    os.write.over(targetDir / s"${topName}.anno.json", annos)
  }

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}
