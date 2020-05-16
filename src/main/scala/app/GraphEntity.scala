package app


trait GraphEntity

abstract class AbstractNode(val elem: List[Int]) extends GraphEntity

case class Start() extends AbstractNode(List.empty[Int]) {
  override def toString: String = "S"
}

case class End() extends AbstractNode(List.empty[Int]) {
  override def toString: String = "E"
}

case class Node(override val elem: List[Int]) extends AbstractNode(elem) {
  override def toString: String = s"(${elem.mkString(", ")})"
}

/** Взвешенное ребро графа */
case class Edge(left: AbstractNode, right: AbstractNode, score: Int) extends GraphEntity {
  override def toString: String = s"$left -$score-> $right"

  def rightToString: String = s"-$score-> $right"
}

/** Структура хранит список ребер графа, который считается "маршрутом".
  * Для него вычисляется оценка из суммы весов ребер */
case class ScoredRoute(score: Int, route: List[Edge])

object ScoredRoute {
  def calcScore(route: List[Edge]) = route.foldRight(0)((e, a) => e.score + a)
}

/** Линки только сверху вниз, граф направленный
  * Т.е. всё, что есть в списке links имеет заданный node слева */
case class LinkedNode(node: AbstractNode, links: List[Edge]) {

  /** Возможность сложить LinkedNode нужна при построении графа */
  def +(that: LinkedNode) = {
    require(node.elem == that.node.elem)
    LinkedNode(node, links ++ that.links)
  }

  override def toString: String =
    s"""
       |$node
       |${links.map("\t" + _.rightToString).mkString("\n")}
       |""".stripMargin

}
