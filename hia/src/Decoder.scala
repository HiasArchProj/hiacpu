package org.chipsalliance.hia

import chisel3._
import chisel3.experimental.hierarchy.{instantiable, public, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}

import chisel3.util.BitPat
import chisel3.util.experimental.decode.{BoolDecodeField, DecodeField, DecodePattern, DecodeTable}
import org.chipsalliance.rvdecoderdb.{Encoding, Instruction, InstructionSet}

object DecoderParameter {
  implicit def rwP: upickle.default.ReadWriter[DecoderParameter] = upickle.default.macroRW[DecoderParameter]
}

case class DecoderParameter() extends SerializableModuleParameter {

}

class DecoderInterface(parameter: DecoderParameter) extends Bundle {}

@instantiable
class Decoder(val parameter: DecoderParameter) extends 
    FixedIORawModule(new DecoderInterface(parameter)) 
    with SerializableModule[DecoderParameter] 
    with Public {

}
