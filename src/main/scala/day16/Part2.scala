package day16

object Part2 extends App with Utils {

  def solve(packet: Packet): Long = packet match {
    case LiteralPacket(_, v) => v
    case OperatorPacket(Header(_, id), subs) => id match {
      case 0 => subs.map(solve).sum
      case 1 => subs.map(solve).product
      case 2 => subs.map(solve).min
      case 3 => subs.map(solve).max
      case 5 => if (solve(subs(0)) > solve(subs(1))) 1 else 0
      case 6 => if (solve(subs(0)) < solve(subs(1))) 1 else 0
      case 7 => if (solve(subs(0)) == solve(subs(1))) 1 else 0
    }
  }

  //import scodec.bits._
  //val input = hex"9C0141080250320F1802104A08".bits
  //val pkt = decode(input)
  //println(solve(pkt))

  run(fs2.io.file.Path("data/day16input.txt"))

}
