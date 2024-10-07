package org.chipsalliance.hia

import chisel3._
import chisel3.util._

/* --- Cache interface --- */
class ICacheIO(xlen: Int) extends Bundle {
  // TODO add valid and ready signal
  val addr = Input(UInt(xlen.W))
  val data = Output(UInt(xlen.W))
  val valid = Output(Bool())
  // val mask = UInt((xlen / 8).W)
}

class DCacheIO(xlen: Int) extends Bundle {
  // TODO add valid and ready signal
  val addr = Input(UInt(xlen.W))
  val data = Output(UInt(xlen.W))
  val wen = Input(Bool())
  val wdata = Input(UInt(xlen.W))
  val valid = Output(Bool())
  val mask = Input(UInt((xlen / 8).W))
}

class readIO(xlen: Int) extends Bundle {
  val req = new Bundle {
    val addr = UInt(xlen.W)
  }
  val resp = Flipped(Valid(new Bundle {
    val data = UInt(xlen.W)
    // val mask = UInt((xlen / 8).W)
  }))
}

class writeIO(xlen: Int) extends Bundle {
  val req = Valid(new Bundle {
    val addr = UInt(xlen.W)
    val data = UInt(xlen.W)
    // val mask = UInt((xlen / 8).W)
  })
  val resp = Flipped(new Bundle {
    val success = Bool()
  })
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
