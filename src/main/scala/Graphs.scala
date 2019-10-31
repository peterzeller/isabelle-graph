import scala.collection.mutable

object Graphs {

  trait GraphInterpreter[Node, Id] {

    def neighbours(node: Node): Iterable[Id]

    def id(node: Node): Id

    def withNeighbours(node: Node, newNeighbours: Set[Id]): Node
  }


  def removeTransitive[Node, Id](nodes: Iterable[Node])(implicit gi: GraphInterpreter[Node, Id]): List[Node] = {
    val byId: Map[Id, Node] = nodes.map(n => gi.id(n) -> n).toMap


    def dfs(v: Node, visited: mutable.Set[Id]): Unit = {
      val vId = gi.id(v)
      if (!visited.contains(vId)) {
        visited.addOne(vId)
        for (n <- gi.neighbours(v))
          dfs(byId(n), visited)
      }
    }

    val newNodes: Iterable[Node] =
      for (u <- nodes) yield {
        val todo = mutable.ListBuffer().addAll(gi.neighbours(u))
        var newNeighbours = Set[Id]()
        while (todo.nonEmpty) {
          val v = todo.head
          val visited = mutable.Set[Id]()
          dfs(byId(v), visited)
          todo.filterInPlace(!visited.contains(_))
          newNeighbours += v
        }
        gi.withNeighbours(u, newNeighbours)
      }

    newNodes.toList

  }
}
