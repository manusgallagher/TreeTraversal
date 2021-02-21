import org.scalatest.funspec.AnyFunSpec

class TreeUtilsTest extends AnyFunSpec {
  describe("countNodes") {
    val testData: Seq[(Tree, Int)] = Seq(
      (Tree(1, List(Tree(2, List(Tree(3, Nil))))), 3),
      (Tree(1, List(Tree(2, List(Tree(3, Nil))), Tree(4, List(Tree(5, Nil))))), 5),
      (Tree(1, List(Tree(2, List(Tree(3, Nil))), Tree(4, List(Tree(5, Nil), Tree(6, Nil))))), 6)
    )

    testData.foreach { case (n, i) =>
      it(s"should return $i for input $n")(assert(TreeUtils.countNodes(n) === i))
    }
  }

  describe("listFromTop") {
    val testData: Seq[(Tree, List[Int])] = Seq(
      (Tree(1, List(Tree(2, List(Tree(3, Nil))))), List(1, 2, 3)),
      (Tree(1, List(Tree(2, List(Tree(3, Nil), Tree(4, Nil))))), List(1, 2, 3, 4)),
      (Tree(1, List(Tree(2, List(Tree(4, Nil), Tree(3, Nil))))), List(1, 2, 4, 3)),
      (Tree(1, List(Tree(2, List(Tree(3, List(Tree(4, Nil), Tree(5, Nil))), Tree(6, List(Tree(7, Nil), Tree(8, Nil))))))), List(1, 2, 3, 4, 5, 6, 7, 8)),
    )

    testData.foreach { case (n, l) =>
      it(s"should return $l for input $n")(assert(TreeUtils.listFromTop(n) === l))
    }
  }

  describe("findValueInTree") {
    val testData: Seq[(Tree, Int, Either[String, String])] = Seq(
      (Tree(1, List(Tree(2, Nil))), 2, Right(s"Found value 2 after searching 2 Trees")),
      (Tree(1, List(Tree(2, Nil))), 3, Left(s"Could not find value 3 after searching 2 Trees")),
      (Tree(1, List(Tree(2, List(Tree(3, List(Tree(4, Nil), Tree(5, Nil))), Tree(6, List(Tree(7, Nil), Tree(8, Nil))))))), 7, Right(s"Found value 7 after searching 7 Trees")),
    )

    testData.foreach { case (tree, value, res) =>
      it(s"should return $res for inputs ($tree, $value)")(assert(TreeUtils.findValueInTree(tree, value) === res))
    }
  }
}
