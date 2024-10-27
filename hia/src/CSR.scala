package org.chipsalliance.hia

import chisel3._
import chisel3.util._
import chisel3.experimental.hierarchy.{instantiable, public, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}


object CSRParameter {
  implicit def rwP: upickle.default.ReadWriter[CSRParameter] =
    upickle.default.macroRW[CSRParameter]
}

case class CSRParameter(xlen: Int) extends SerializableModuleParameter {
  // Supports machine & user modes
  val PRV_U = 0x0.U(2.W)
  val PRV_M = 0x3.U(2.W)

//   val mcpuid = 0xf00.U(12.W)
//   val mimpid = 0xf01.U(12.W)
  val MHARTID = 0xf14.U(12.W)
  // Machine Trap Setup
  val MSTATUS = 0x300.U(12.W)
  val MTVEC = 0x305.U(12.W)
//   val mtdeleg = 0x302.U(12.W)
  val MIE = 0x304.U(12.W)
//   val mtimecmp = 0x321.U(12.W)

  // Machine Timers and Counters
//   val mtime = 0x701.U(12.W)
//   val mtimeh = 0x741.U(12.W)
  // Machine Trap Handling
  val MSCRATCH = 0x340.U(12.W)
  val MEPC = 0x341.U(12.W)
  val MCAUSE = 0x342.U(12.W)
//   val mbadaddr = 0x343.U(12.W)
  val MIP = 0x344.U(12.W)

  // Machine HITF
//   val mtohost = 0x780.U(12.W)
//   val mfromhost = 0x781.U(12.W)
}


class CSRInterface(parameter: CSRParameter) extends Bundle { 
  val clock = Input(Clock())
  val reset = Input(Bool())

  val csr = new CSRFileIO(parameter.xlen)
}

@instantiable
class CSR(val parameter: CSRParameter)
    extends FixedIORawModule(new CSRInterface(parameter))
    with SerializableModule[CSRParameter]
    with Public
    with ImplicitClock
    with ImplicitReset {
  override protected def implicitClock: Clock = io.clock
  override protected def implicitReset: Reset = io.reset
  val xlen = parameter.xlen


  val MTIP = RegInit(false.B)
  val HTIP = false.B
  val STIP = false.B
  val MTIE = RegInit(false.B)
  val HTIE = false.B
  val STIE = false.B
  val MSIP = RegInit(false.B)
  val HSIP = false.B
  val SSIP = false.B
  val MSIE = RegInit(false.B)
  val HSIE = false.B
  val SSIE = false.B
  val mip = Cat(0.U((xlen - 8).W), MTIP, HTIP, STIP, false.B, MSIP, HSIP, SSIP, false.B)
  val mie = Cat(0.U((xlen - 8).W), MTIE, HTIE, STIE, false.B, MSIE, HSIE, SSIE, false.B)  

  val PRV = RegInit(parameter.PRV_M)
  val PRV1 = RegInit(parameter.PRV_M)
  val PRV2 = 0.U(2.W)
  val PRV3 = 0.U(2.W)
  val IE = RegInit(false.B)
  val IE1 = RegInit(false.B)
  val IE2 = false.B
  val IE3 = false.B
  // virtualization management field
  val VM = 0.U(5.W)
  // memory privilege
  val MPRV = false.B
  // extention context status
  val XS = 0.U(2.W)
  val FS = 0.U(2.W)
  val SD = 0.U(1.W)
  val mstatus = Cat(SD, 0.U((xlen - 23).W), VM, MPRV, XS, FS, PRV3, IE3, PRV2, IE2, PRV1, IE1, PRV, IE)

  val mepc = Reg(UInt(xlen.W))
  val mcause = Reg(UInt(xlen.W))
  val mscratch = Reg(UInt(xlen.W))
  val mtvec = Reg(UInt(xlen.W))
  val mhartid = RegInit(0.U(xlen.W))


  val csrFile = Seq(
    // BitPat(parameter.cycle) -> cycle,
    // BitPat(parameter.time) -> time,
    // BitPat(parameter.instret) -> instret,
    // BitPat(parameter.cycleh) -> cycleh,
    // BitPat(parameter.timeh) -> timeh,
    // BitPat(parameter.instreth) -> instreth,
    // BitPat(parameter.cyclew) -> cycle,
    // BitPat(parameter.timew) -> time,
    // BitPat(parameter.instretw) -> instret,
    // BitPat(parameter.cyclehw) -> cycleh,
    // BitPat(parameter.timehw) -> timeh,
    // BitPat(parameter.instrethw) -> instreth,
    // BitPat(parameter.mcpuid) -> mcpuid,
    // BitPat(parameter.mimpid) -> mimpid,
    // BitPat(parameter.mhartid) -> mhartid,
    BitPat(parameter.MTVEC) -> mtvec,
    // BitPat(parameter.mtdeleg) -> mtdeleg,
    BitPat(parameter.MIE) -> mie,
    // BitPat(parameter.mtimecmp) -> mtimecmp,
    // BitPat(parameter.mtime) -> time,
    // BitPat(parameter.mtimeh) -> timeh,
    BitPat(parameter.MSCRATCH) -> mscratch,
    BitPat(parameter.MEPC) -> mepc,
    BitPat(parameter.MCAUSE) -> mcause,
    // BitPat(parameter.mbadaddr) -> mbadaddr,
    BitPat(parameter.MIP) -> mip,
    // BitPat(parameter.mtohost) -> mtohost,
    // BitPat(parameter.mfromhost) -> mfromhost,
    BitPat(parameter.MSTATUS) -> mstatus,
    BitPat(parameter.MHARTID) -> mhartid
  )

  io.csr.rdata := Lookup(io.csr.csr_addr, 0.U, csrFile).asUInt

  when(io.csr.wen) {
    when(io.csr.csr_addr === parameter.MSTATUS) {
    PRV1 := io.csr.wdata(5, 4)
    IE1 := io.csr.wdata(3)
    PRV := io.csr.wdata(2, 1)
    IE := io.csr.wdata(0)
    }
    .elsewhen(io.csr.csr_addr === parameter.MIP) {
        MTIP := io.csr.wdata(7)
        MSIP := io.csr.wdata(3)
    }
    .elsewhen(io.csr.csr_addr === parameter.MIE) {
        MTIE := io.csr.wdata(7)
        MSIE := io.csr.wdata(3)
    }
    .elsewhen(io.csr.csr_addr === parameter.MSCRATCH) { mscratch := io.csr.wdata }
    .elsewhen(io.csr.csr_addr === parameter.MEPC) { mepc := Cat(io.csr.wdata(xlen - 1, 2), 0.U(2.W)) }
    .elsewhen(io.csr.csr_addr === parameter.MCAUSE) { mcause := io.csr.wdata & (BigInt(1) << (xlen - 1) | 0xf).U }
    }
}
