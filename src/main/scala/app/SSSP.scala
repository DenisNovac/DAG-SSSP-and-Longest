package app

import app.GraphBuilder.DAG

object SSSP {

  /**
    * Топологическая сортировка (Topological Sort, TopSort):
    *
    * - Взять непосещённую ноду (любую)
    * - Выполнить Depth First Search, которая ищет только по непосещённым нодам (доступным на выбранной)
    * - Рекурсивной вызывать пока не дойдешь до ситуации, когда текущая нода не имеет непосещённых
    * - Добавить ноду в порядок слева, вернуться наружу;
    *
    * https://youtu.be/eL-KzMXSXXI
    * @param graph - Направленный ациклический граф
    */
  def topSort(graph: DAG): List[LinkedNode] = {

    val t0 = System.nanoTime()

    val topOrderReversed = scala.collection.mutable.ArrayBuffer.empty[LinkedNode]
    val visited          = scala.collection.mutable.Set.empty[AbstractNode]
    topOrderReversed += LinkedNode(End(), List.empty) // очевиднее всего добавить End сюда

    /** Deep First Search */
    def DFS(node: LinkedNode): Unit =
      //println(s"In ${node}")
      node.node match {

        // по идее, сюда можно попасть только если начать с End, т.к. дальше проверяются edge.right
        case End() => ()

        case _ =>
          // перебираем все грани текущей ноды и вносим их в visited
          for {
            edge <- node.links
          } yield {
            edge.right match {
              case End()                             => ()
              case _ if visited.contains(edge.right) => ()
              case _ =>
                DFS(graph(edge.right))
                visited += edge.right
            }
          }
          // когда грани кончились - текущая нода добавляется в топологический порядок
          topOrderReversed += node
      }

    DFS(graph(Start()))
    val topOrder = topOrderReversed.reverse.toList

    println(s"Topsort done in ${(System.nanoTime() - t0) / 1000000} ms")
    //println(topOrder.map(_.node).mkString("\n"))

    topOrder
  }



  /**
    * Найти кратчайший путь от старта до всех узлов графа (Single Source Shortest Path(SSSP)):
    *
    * - Взять топологическую сортировку графа;
    * - Лучшую дистанцию до каждой ноды взять за INF;
    * - Дистанция до стартовой ноды равна нулю;
    * - Дойти до какой-нибудь ноды. Обновить дистанцию до неё;
    * - Когда обошли все связи текущей ноды - идем в следующую согласно топологическому порядку
    * - Если начиная с другой ноды нашли ту, что уже есть в списке посещённых - сравниваем длины и обновляем путь если нужно
    *
    *
    *
    * Находясь в некоторой "текущей" ноде смотрим лучший путь до неё + путь до каждой из граней, записываем "лучшие"
    * потом идём на следующие грани
    *
    * Храню вместе с лучшими очками роуты между лучшими гранями
    *
    * Например:
    *
    * A -> B = 1  (B -> ( 1 (A,B) ))
    * B -> C = 3  (C -> ( 4 (B,C) ))
    * C -> D = 3  (D -> ( 7 (C,D) ))
    *
    * Потом складываю их и получаю лучший роут

    * https://youtu.be/TXkDpqjDMHA
    * @param topOrder Направленный ациклический граф
    */
  private def calculateSomeOnDag(topOrder: List[LinkedNode], name: String, inf: Int) = {
    val t0 = System.nanoTime()
    var countOfEdges = 0

    // Заполняем дистанции большим числом для сравнения
    val bestScores: collection.mutable.Map[AbstractNode, ScoredRoute] = collection.mutable.Map() ++
      topOrder.map { ln =>
        ln.node -> ScoredRoute(inf, List.empty[Edge])
      }.toMap

    // Первая дистанция - дистанция до старта, она равна нулю
    bestScores += topOrder.head.node -> ScoredRoute(0, List.empty[Edge])

    for {
      // всё равно берем первую потому что мы будем смотреть её грани
      // граней до первой ноды нет вообще, поэтому мы записывали её вручную выше
      node <- topOrder
    } yield {
      for {
        edge <- node.links
      } yield {
        countOfEdges += 1
        edge.right match {
          case abs =>
            // считаем дорогу до этой грани - это сумма очков предыдущей ноды и веса грани
            val scoreFromAllRoutes = bestScores(node.node).score + edge.score
            val bestScore          = bestScores(abs).score
            // Если нашли результат лучше
            if (scoreFromAllRoutes < bestScore) {
              bestScores += abs -> ScoredRoute(scoreFromAllRoutes, List(edge))
            }

        }
      }

    }

    /** Теперь разворачиваем от End в Start (справа-налево) */
    val route = collection.mutable.ArrayBuffer.empty[Edge]

    def recFoldBestRoutes(right: AbstractNode): Unit = {
      val pathTo: ScoredRoute = bestScores(right)

      pathTo.route match {
        case list if list.nonEmpty =>
          val edge: Edge                  = pathTo.route.head
          val nodeAtTheLeft: AbstractNode = edge.left
          recFoldBestRoutes(nodeAtTheLeft)
          route += edge
        case Nil => ()
      }
    }

    recFoldBestRoutes(topOrder.last.node)
    println(s"Edges passed $countOfEdges")
    println(s"$name path found in ${(System.nanoTime() - t0) / 1000000} ms")

    val first = topOrder.head.links.filter(_.right == route.head.left).head
    val result = first +: route

    println(result.mkString("\n"))

    result.toList
  }


  def calculateSsspOnDag(topOrder: List[LinkedNode]) = calculateSomeOnDag(topOrder, "Shortest", 999_999)


  def calculateLongestPathOnDag(topOrder: List[LinkedNode]) = {

    val reversedTopOrder = topOrder.map { ln =>
      val node = ln.node
      val edgesReversed = ln.links.map(edge => Edge(edge.left, edge.right, edge.score * -1))
      LinkedNode(node, edgesReversed)
    }

    calculateSomeOnDag(reversedTopOrder, "Longest", -999_999)
  }


}
