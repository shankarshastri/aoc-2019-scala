import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._
import scala.io.Source

object Day05 extends App {

  @tailrec
  def calcPartA(a: Array[Int], i: Int = 0): Int = {
    a(i) match {
      case 1 => calcPartA(a.updated(a(i + 3), a(a(i+1)) + a(a(i + 2))), i + 4)
      case 2 => calcPartA(a.updated(a(i + 3), a(a(i+1)) *  a(a(i + 2))), i + 4)
      case 99 => a(0)
    }
  }

  def solveA(s: String): Int = {
    val a = s.split(",").map(_.toInt)
    val updatedA = a.updated(1, 12).updated(2, 2)
    calcPartA(updatedA)
  }


  def solveB(s: String, ex: Int = 19690720): Int = {
    val a = s.split(",").map(_.toInt)
    val r = (1 to 99)
    val res = r.flatMap(e => r.map(e1 => (e, e1))).to(LazyList).par.map(e => {
      (e._1, e._2, calcPartA(a.updated(1, e._1).updated(2, e._2)))
    }).filter(_._3 == ex)
    val resL = res.last
    resL._1 * 100 + resL._2
  }

  def solveBTail(s: String, ex: Int = 19690720): Int = {
    @tailrec
    def solveBHelper(s: String, i: Int = 1, j: Int = 1, ex: Int): Int = {
      val a = s.split(",").map(_.toInt)
      val res = calcPartA(a.updated(1, i).updated(2, j))
      (i, j, res) match {
        case (i, j, res) if res == ex => i * 100 + j
        case (i, j, _) if j > 99 && i <= 99 => solveBHelper(s, i+1, 0, ex)
        case (i, j, _) if j <= 99 => solveBHelper(s, i, j+1, ex)
        case _ => -1
      }
    }
    solveBHelper(s, ex = ex)
  }

  val readmeText: Iterator[String] = Source.fromResource("day-02.in").getLines
  val t = readmeText.mkString
  println(solveA(t))
  println(solveB(t))
  println(solveBTail(t))
}
