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
  SSSP.calculateLongestPathOnDag(topOrder)

  println()
  Old.calcLongestFromAll(graph)

  //val sssp = SSSP.calculateSsspOnDag(graph, topOrder, Start())
}
