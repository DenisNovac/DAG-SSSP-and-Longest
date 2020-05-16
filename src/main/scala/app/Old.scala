package app

/** Поиск длиннейшего пути перебором */
object Old {

  def calcLongestFromAll(graph: Map[AbstractNode, LinkedNode]): ScoredRoute = {
    val t0    = System.nanoTime()
    val start = graph(Start())

    var countOfEdges = 0

    val cache: collection.mutable.Map[LinkedNode, ScoredRoute] =
      scala.collection.mutable.Map.empty[LinkedNode, ScoredRoute] // кэш хранит единственное наиболее длинное ребро

    def longestForNode(node: LinkedNode, acc: List[Edge], score: Int): ScoredRoute =
      cache.get(node) match {
        case Some(value) => value
        case None =>
          val longest = {
            for {
              edge <- node.links
            } yield {
              edge.right match {
                case End() =>
                  ScoredRoute(score, acc :+ edge)
                case n: Node =>
                  val linked = graph(n)

                  countOfEdges += 1
                  // longest хранит весь путь от старта, который мы будем возвращать
                  longestForNode(linked, acc :+ edge, score + edge.score)

              }
            }
          }.maxBy(_.score)
          /*val l = longest.route.dropWhile(_.left != node.node)
          cache += node -> ScoredRoute(l.foldRight(0)((e,a) => e.score + a), l)*/
          //cache += node -> longest
          longest
      }

    val longest = longestForNode(start, List.empty[Edge], 0)

    println()
    println(longest.route.mkString("\n"))
    println(s"${(System.nanoTime() - t0) / 1000000} ms")
    println("edges passed: " + countOfEdges)
    //println(cache)

    longest
  }
}
