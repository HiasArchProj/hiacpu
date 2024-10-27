package org.chipsalliance.hia

import chisel3._
import chisel3.util._

/* --- Cache interface --- */
class ICacheIO(xlen: Int) extends Bundle {
  // TODO add valid and ready signal
  val raddr = Input(UInt(xlen.W))
  val rdata = Output(UInt(xlen.W))
  val rvalid = Output(Bool())
  // val ren = Input(Bool())
}

class DCacheIO(xlen: Int) extends Bundle {
  // TODO modify HIATestBench and DPI-C
  val raddr = Input(UInt(xlen.W))
  val rdata = Output(UInt(xlen.W))
  val ren = Input(Bool())
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

class FetchStageMessage(xlen: Int) extends Bundle {
  val pc = Output(UInt(xlen.W))
}

class ExecuteStageMessage(xlen: Int, decoderParameter: DecoderParameter) extends Bundle {
  val inst = Output(UInt(xlen.W))
  val pc = Output(UInt(xlen.W))
  val alu_out = Output(UInt(xlen.W))
  val st_data = Output(UInt(xlen.W))
  val csr_data = Output(UInt(xlen.W))

  val ld_type = Output(UInt(decoderParameter.LD_TYPE_LEN.W))
  val st_type = Output(UInt(decoderParameter.ST_TYPE_LEN.W))
  val wb_sel = Output(UInt(decoderParameter.WB_SEL_LEN.W))
}

class ExecuteStageBypassMessage(xlen: Int, pc_sel_len: Int) extends Bundle {
  val pc_sel = UInt(pc_sel_len.W)
  val taken = Bool()
  val alu_out = UInt(xlen.W)
}

class WriteBackStageBypassMessage(xlen: Int, wb_sel_len: Int) extends  Bundle {
  val wb_sel = UInt(wb_sel_len.W)
  val wb_en = Bool()
  val wb_rd_addr = UInt(5.W)
  val wb_data = UInt(xlen.W)
}


class regReadReqIO(rlen: Int) extends Bundle {
  val addr1 = UInt(rlen.W)
  val addr2 = UInt(rlen.W)
}

class regReadRespIO(xlen: Int) extends Bundle {
  val data1 = UInt(xlen.W)
  val data2 = UInt(xlen.W)
}

class regReadIO(xlen: Int) extends Bundle {
  val req = Flipped(new regReadReqIO(5))
  val rsp = new regReadRespIO(xlen)
}

class regWriteReqIO(xlen: Int, rlen: Int) extends Bundle {
  val wen = Bool()
  val waddr = UInt(rlen.W)
  val wdata = UInt(xlen.W)
}

class regWriteIO(xlen: Int) extends Bundle {
  val req = Flipped(new regWriteReqIO(xlen, 5))
}

class RegFileIO(xlen: Int) extends Bundle {
  val r = new regReadIO(xlen)
  val w = new regWriteIO(xlen)
}

class CSRFileIO(xlen: Int) extends Bundle {
  val csr_addr = Input(UInt(12.W))
  val rdata = Output(UInt(xlen.W))
  val wdata = Input(UInt(xlen.W))
  val wen = Input(Bool())
}