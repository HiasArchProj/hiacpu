
package org.chipsalliance.hia

import chisel3._
import chisel3.util._

class ImemPortIO(width: Int)  extends Bundle {
    val addr = Input(UInt(width.W))
    val inst = Input(UInt(width.W))
}

class DmemPortIO(width: Int) extends Bundle {
    val addr = Input(UInt(width.W))
    val rdata = Input(UInt(width.W))
    val wen = Input(Bool())
    val wdata = Input(UInt(width.W))
}
