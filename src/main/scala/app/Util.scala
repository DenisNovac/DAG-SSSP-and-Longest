package app

import scala.annotation.tailrec

object Util {

  /** Делает сплиты из последовательностей:
    * 1,2,3 =>
    * (1), (2), (3), (1,2), (2,3), (1,2,3) */
  def splitMany(l: List[Int], maxSize: Int): List[List[Int]] = {
    val newMaxSize =
      if (maxSize > l.length) l.length
      else maxSize

    for {
      size <- 1 to newMaxSize
    } yield split(l, size)
  }.toList.flatten

  def split(l: List[Int], size: Int) = {
    @tailrec
    def recSplit(l: List[Int], size: Int, acc: List[List[Int]]): List[List[Int]] = l match {
      case Nil => acc

      case list if list.length > size =>
        val split = list.splitAt(size)
        //recSplit(split._2, size, split._1 :: acc)
        recSplit(list.drop(1), size, split._1 :: acc)

      case list => list :: acc
    }

    if (size == 1) l.map(e => List(e))
    else {
      recSplit(l, size, Nil).reverse
    }
  }
}
