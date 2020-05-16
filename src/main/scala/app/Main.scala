package app

import app.Util.splitMany
import app.GraphBuilder._
import scala.language.postfixOps

object Main extends App {
  val elements      = 1 to 15 toList
  val maxSplitsSize = 10

  val splits = splitMany(elements, maxSplitsSize)
  val order  = elements

  val graph: DAG = buildGraph(order, splits)


  //printGraph(graph)

  val topOrder = SSSP.topSort(graph)




  println()
  //SSSP.calculateSsspOnDag(topOrder)
  val longest = SSSP.calculateLongestPathOnDag(topOrder)
  println(longest.foldRight(0)((e, acc) => e.score + acc) * -1)

  /*println()
  val longest_old = Old.calcLongestFromAll(graph)
  println(longest_old.score)*/

  //val sssp = SSSP.calculateSsspOnDag(graph, topOrder, Start())
}
