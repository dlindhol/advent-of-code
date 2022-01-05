package day16

import scala.util.matching.Regex

import cats.effect.IO
import fs2._
import scodec.Decoder
import scodec.DecodeResult
import scodec.bits._
import scodec.codecs._
import scodec.Attempt.Failure
import scodec.Attempt.Successful
import scodec.Err
import util.AocUtils

trait Utils extends AocUtils {

  val pattern: Regex = raw"\w+".r

  type Transmission = BitVector
  type Row = Transmission

  case class Header(version: Int, typeId: Int)

  trait Packet { def header: Header }
  case class LiteralPacket(header: Header, value: Long) extends Packet
  case class OperatorPacket(header: Header, subPackets: List[Packet]) extends Packet

  case class C4(continue: Boolean, bv: BitVector)

  type Data = Packet
  type Result = Long

  //----

  def parseRow(s: String): Transmission = BitVector.fromValidHex(s)

  val header: Decoder[Header] = (uint(3) :: uint(3)).as[Header]
  val c4: Decoder[C4] = (bool :: bits(4)).as[C4]

  val literal: Decoder[Long] = (bits: BitVector) => {
    var remainder = bits
    var acc = BitVector.empty
    var cont = true
    var err: Option[Err] = None
    while (cont) {
      c4.decode(remainder) match {
        case Successful(DecodeResult(C4(c, bv), r)) =>
          //println(c, bv, r)
          cont = c
          remainder = r
          acc = acc ++ bv
        case Failure(e) =>
          println(e)
          println(remainder)
          err = Some(e)
          cont = false
      }
    }
    if (err.nonEmpty) Failure(err.get)
    else ulong(acc.length.toInt).decode(acc ++ remainder)
  }

  val literalPacket: Decoder[LiteralPacket] =
    (header.decodeOnly :: literal.decodeOnly).as[LiteralPacket]

  val subPackets: Decoder[List[Packet]] = {
    bool(1).flatMap { b =>
      if (b) listOfN(uint(11), packet.decodeOnly)
      else variableSizeBits(uint(15), packets.decodeOnly)
    }
  }

  val operatorPacket: Decoder[OperatorPacket] =
    (header.decodeOnly :: subPackets.decodeOnly).as[OperatorPacket]

  val packet: Decoder[Packet] = (bits: BitVector) => {
    header.decode(bits).flatMap { dr =>
      //println(dr.value)
      dr.value match {
        case Header(_, t) => t match {
          case 4 => literalPacket.decode(bits)
          case _ => operatorPacket.decode(bits)
        }
      }
    }
  }

  val packets: Decoder[List[Packet]] = list(packet.decodeOnly)

  def showBits(bits: BitVector): Unit = {
    (0 until bits.length.toInt).foreach { i =>
      if (bits.get(i)) print("1")
      else print("0")
    }
    println
  }

  //---

  def process: Pipe[IO, Row, Data] = _.map(decode)

  def decode(transmission: Transmission): Packet = packet.decodeValue(transmission).require

  def solve: Pipe[IO, Data, Result] = _.map(solve)

  def solve(packet: Packet): Long

}
