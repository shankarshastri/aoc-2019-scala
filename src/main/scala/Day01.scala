import scala.annotation.tailrec
import scala.io.Source

object Day01 extends App {

  def calc(i: Int): Int = Math.floorDiv(i, 3) - 2

  @tailrec
  def calcTillNeg(i: Int, acc: Int = 0): Int = {
    if(i <= 0) acc
    else {
      val l = calc(i)
      if(l < 0) acc
      else calcTillNeg(l, acc + l)
    }
  }
  val readmeText: Iterator[String] = Source.fromResource("day-01.in").getLines
  print(readmeText.map(_.toInt).map(calcTillNeg(_)).sum)
}
