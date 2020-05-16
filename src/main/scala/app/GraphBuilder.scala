package app

import scala.collection.immutable
import scala.util.Random

object GraphBuilder {

  /** Directed Acyclic Graph хранит карту (элемент -> связи элемента) */
  type DAG = Map[AbstractNode, LinkedNode]

  def printGraph(graph: DAG): Unit = println(graph.values.mkString(""))

  def buildGraph(order: List[Int], splits: List[List[Int]]): DAG = {

    val t0 = System.nanoTime()

    /** Рандомно вычисляем веса для графа */
    val scores: Map[List[Int], Int] = splits.map(l => l -> Random.nextInt(splits.length)).toMap

    /** Группы нужны чтобы итерироваться по ним согласно порядку order */
    val groups: Map[Int, List[List[Int]]] = splits.groupBy(l => l.head)

    val edges = {
      for {
        o <- order
      } yield for {
        group <- groups(o)
      } yield {
        if (o == order.last) {
          List(Edge(Node(group), End(), 0))
        } else {
          val last = group.last + 1
          if (last == order.last + 1) List(Edge(Node(group), End(), 0))
          else groups(last).map(rights => Edge(Node(group), Node(rights), scores(rights)))
        }
      }

    }.flatten.flatten

    // Дописываем стартовые отношения S -> ...
    val full = (edges ++ edges
      .groupBy(e => e.left.elem.head)(order.head)
      .map(e => Edge(Start(), e.left, scores(e.left.elem)))
      .distinct)
      .map(edge => LinkedNode(edge.left, List(edge))) // Простой маппинг в класс, который можно потом фолднуть

    // Складываем ноды от одного и того же элемента
    val linkedNodes: Iterable[LinkedNode] = for {
      group <- full.groupBy(n => n.node)
    } yield {
      // Дроп первого элемента, который будем считать за дефолтный при фолде
      group._2.drop(1).foldRight(group._2.head)(_ + _)
    }

    //println(linkedNodes.mkString(""))
    println("Edges in graph: " + linkedNodes.foldRight(0)((n, acc) => n.links.length + acc))

    /** Эта карта содержит ноды и ссылки на их LinkedNodes.
      * Она нужна чтобы из нод Edge находить следующие Edge
      * */
    val nodesMap: DAG = linkedNodes.map(ln => (ln.node, ln)).toMap
    //println(nodesMap.mkString("---------\n"))

    println(s"DAG build done in ${(System.nanoTime() - t0) / 1000000} ms")
    nodesMap
  }
}
