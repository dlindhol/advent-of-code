package day16

object Part1 extends App with Utils {

  def solve(packet: Packet): Long = sumVersions(packet)

  def sumVersions(packet: Packet): Long = packet match {
    case LiteralPacket(Header(v, _), _) => v
    case OperatorPacket(Header(v, _), subPackets) =>
      v + subPackets.map(sumVersions).sum
  }

  //import scodec.bits._
  //val input = hex"A0016C880162017C3686B18A3D4780".bits
  //val pkt = decode(input)
  //println(sumVersions(pkt))

  run(fs2.io.file.Path("data/day16input.txt"))

}
