import Graphs.GraphInterpreter
import org.junit.{Assert, Test}

class GraphTests {

  implicit def gi[T]: GraphInterpreter[(T, Set[T]), T] = new GraphInterpreter[(T, Set[T]), T] {
    override def neighbours(node: (T, Set[T])): Iterable[T] = node._2

    override def id(node: (T, Set[T])): T = node._1

    override def withNeighbours(node: (T, Set[T]), newNeighbours: Set[T]): (T, Set[T]) = (node._1, newNeighbours)
  }

  @Test
  def graphtest(): Unit = {
    val nodes: List[(String, Set[String])] = List(
      "a" -> Set("b", "c", "d"),
      "b" -> Set("d"),
      "c" -> Set("d"),
      "d" -> Set()
    )

    val newNodes = Graphs.removeTransitive(nodes)

    val expected: List[(String, Set[String])] = List(
          "a" -> Set("b", "c"),
          "b" -> Set("d"),
          "c" -> Set("d"),
          "d" -> Set()
        )
    Assert.assertEquals(expected, newNodes)

  }

}
