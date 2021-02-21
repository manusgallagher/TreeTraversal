import scala.annotation.tailrec

object TreeUtils {

  def findValueInTree(tree: Tree, valueToFind: Int): Either[String, String] = {
    @tailrec
    def loop(t: List[Tree], v: Int, statesVisited: List[Int]): Either[String, String] =
      t match {
        case Nil                      => Left(s"Could not find value $v after searching ${statesVisited.length} Trees")
        case n :: _ if n.value == v   => Right(s"Found value $v after searching ${statesVisited.length + 1} Trees")
        case Tree(value, edges) :: ls => loop(edges ++ ls, valueToFind, statesVisited :+ value)
      }

    loop(List(tree), valueToFind, List.empty)
  }

  def listFromTop(tree: Tree): List[Int] = {
    @tailrec
    def loop(t: List[Tree], acc: List[Int]): List[Int] =
      t match {
        case Nil                      => acc
        case Tree(value, edges) :: ls => loop(edges ++ ls, acc :+ value)
      }

    loop(List(tree), List.empty)
  }

  def countNodes(tree: Tree): Int = {
    @tailrec
    def loop(t: List[Tree], acc: Int): Int =
      t match {
        case Nil        => acc
        case tree :: ls => loop(tree.edges ++ ls, acc + 1)
      }

    loop(List(tree), 0)
  }
}
