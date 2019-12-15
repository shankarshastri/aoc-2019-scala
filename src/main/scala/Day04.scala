import scala.concurrent.{Await, Future}

object Day04 extends App {

  def containsContinuos(i: Int, prev:Int = -1, res: Boolean = false): Boolean = {
    if(res) true
    else if(i == 0) false
    else containsContinuos(i/10, i % 10, prev == i % 10)
  }

  def containsContinuosPart2(i: Int, c: Int = 0, prev: Int = -1,
                             listOfTuples: List[(Int, Int)] = List.empty[(Int, Int)],
                             index: Int = -1): Boolean = {
    if(i == 0) listOfTuples.exists(_._2 == 2)
    else {
      if(prev == i % 10)
        containsContinuosPart2(i/10, c+1, i % 10, listOfTuples.updated(index, (i % 10, c + 1)), index)
      else
        containsContinuosPart2(i/10, 1, i % 10,  listOfTuples :+ (i % 10, 1)  , index + 1)
    }
  }

  def increasingSeq(i: Int): Boolean = {
    i.toString.map(_.toString.toInt).sliding(2).toList.map(e => e.head <= e.tail.head).reduce(_ && _)
  }

  def validate(i: Int): Boolean =
    containsContinuos(i) && increasingSeq(i)

  def validatePartB(i: Int): Boolean =
    containsContinuosPart2(i) && increasingSeq(i)

  def calcPartA(a: Int, b: Int): Int =
    (a to b).count(validate)

  def calcPartB(a: Int, b: Int): Int = {
    (a to b).count(validatePartB)
  }

  println(calcPartA(128392, 643281))
  println(calcPartB(128392, 643281))

}
