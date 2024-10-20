// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2024 Jiuyang Liu <liu@jiuyang.me>

import mill._
import mill.scalalib._
import mill.define.{Command, TaskModule}
import mill.scalalib.publish._
import mill.scalalib.scalafmt._
import mill.scalalib.TestModule.Utest
import mill.util.Jvm
import coursier.maven.MavenRepository
import $file.dependencies.chisel.build
import $file.dependencies.rvdecoderdb.common
import $file.common

object deps {
  val scalaVer = "2.13.15"
  val mainargs = ivy"com.lihaoyi::mainargs:0.5.0"
  val oslib = ivy"com.lihaoyi::os-lib:0.10.0"
  val upickle = ivy"com.lihaoyi::upickle:3.3.1"
}

object chisel extends Chisel

trait Chisel extends millbuild.dependencies.chisel.build.Chisel {
  def crossValue = deps.scalaVer
  override def millSourcePath = os.pwd / "dependencies" / "chisel"
}

object rvdecoderdb extends RVDecoderDB

trait RVDecoderDB extends millbuild.dependencies.rvdecoderdb.common.RVDecoderDBJVMModule with ScalaModule {
  def scalaVersion            = T(deps.scalaVer)
  def osLibIvy                = deps.oslib
  def upickleIvy              = deps.upickle
  override def millSourcePath = os.pwd / "dependencies" / "rvdecoderdb" / "rvdecoderdb"
}

object hia extends HIA
trait HIA extends millbuild.common.HasChisel with millbuild.common.HasRVDecoderDB with  ScalafmtModule {
  def scalaVersion = T(deps.scalaVer)

  def chiselModule = Some(chisel)
  def chiselPluginJar = T(Some(chisel.pluginModule.jar()))
  def chiselIvy = None
  def chiselPluginIvy = None

  def rvdecoderdbModule = rvdecoderdb
  def riscvOpcodesPath  = T.input(PathRef(os.pwd / "dependencies" / "riscv-opcodes"))
}

object elaborator extends Elaborator
trait Elaborator extends millbuild.common.ElaboratorModule with ScalafmtModule {
  def scalaVersion = T(deps.scalaVer)

  def panamaconverterModule = panamaconverter

  def circtInstallPath =
    T.input(PathRef(os.Path(T.ctx().env("CIRCT_INSTALL_PATH"))))

  def generators = Seq(hia)

  def mainargsIvy = deps.mainargs

  def chiselModule = Some(chisel)
  def chiselPluginJar = T(Some(chisel.pluginModule.jar()))
  def chiselPluginIvy = None
  def chiselIvy = None
}

object panamaconverter extends PanamaConverter
trait PanamaConverter extends millbuild.dependencies.chisel.build.PanamaConverter {
  def crossValue = deps.scalaVer

  override def millSourcePath =
    os.pwd / "dependencies" / "chisel" / "panamaconverter"

  def scalaVersion = T(deps.scalaVer)
}
