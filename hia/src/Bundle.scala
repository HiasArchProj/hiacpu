package org.chipsalliance.hia

import chisel3._
import chisel3.util._

/* --- Cache interface --- */
class ICacheIO(xlen: Int) extends Bundle {
  // TODO add valid and ready signal
  val raddr = Input(UInt(xlen.W))
  val rdata = Output(UInt(xlen.W))
  val ren = Output(Bool())
}

class DCacheIO(xlen: Int) extends Bundle {
  // TODO modify HIATestBench and DPI-C
  val raddr = Input(UInt(xlen.W))
  val rdata = Output(UInt(xlen.W))
  val ren = Output(Bool())
  val waddr = Input(UInt(xlen.W))
  val wen = Input(Bool())
  val wdata = Input(UInt(xlen.W))
  val wmask = Input(UInt((xlen / 8).W)) // implement sb sh sw
}

class readReqIO(xlen: Int) extends Bundle {
  val addr = UInt(xlen.W)
}

class readRespIO(xlen: Int) extends Bundle {
  val data = UInt(xlen.W)
}

class readIO(xlen: Int) extends Bundle {
  val req = new readReqIO(xlen)
  val resp = Flipped(Valid(new readRespIO(xlen)))
}

class writeReqIO(xlen: Int) extends Bundle {
  val addr = UInt(xlen.W)
  val data = UInt(xlen.W)
}

class writeRespIO(xlen: Int) extends Bundle {
  val success = Bool()
}

class writeIO(xlen: Int) extends Bundle {
  val req = Valid(new writeReqIO(xlen))
  val resp = Flipped(new writeRespIO(xlen))
}

// TODO: refactor to use [[org.chipsalliance.amba.axi4.bundle.AXI4ROIrrevocable]]
class instructionFetchAXI(xlen: Int) extends Bundle {
  val r = new readIO(xlen)
}

// TODO: refactor to use [[org.chipsalliance.amba.axi4.bundle.AXI4RWIrrevocable]]
class loadStoreAXI(xlen: Int) extends Bundle {
  val r = new readIO(xlen)  
  val w = new writeIO(xlen)
}

object Cause {
  val InstAddrMisaligned = 0x0.U
  val IllegalInst = 0x2.U
  val Breakpoint = 0x3.U
  val LoadAddrMisaligned = 0x4.U
  val StoreAddrMisaligned = 0x6.U
  val Ecall = 0x8.U
}
