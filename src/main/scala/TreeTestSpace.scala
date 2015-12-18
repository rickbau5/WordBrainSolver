
import com.bau5.wordbrainsolver.{Tree, Node}
import scala.io.StdIn


/**
  * Created by Rick on 12/2/15.
  */
// scalastyle:off
object TreeTestSpace {
  type BranchTraversal[A] = Seq[Node[A]]

  def main(args: Array[String]): Unit = {
    val tree = gimmeDatTree()
    val traversals = getValidBranchTraversals(tree, 3)
    val grouped = traversals.groupBy(_.head.data)
    val trees = grouped.map { case (root, rest) =>
        val newTree = new Tree(root)
        rest foreach (e => buildTree(newTree, e.tail))
        newTree
    }

    trees foreach traverseTree
  }

  def buildTree[A](tree: Tree[A], traversal: BranchTraversal[A]): Tree[A] = {
    def evaluateNodes(newTreeNode: Node[A], trav: BranchTraversal[A]): Tree[A] = trav match {
      case head :: Nil =>
        if (newTreeNode.children.forall(_.data != head.data)) {
          newTreeNode.addChild(head.data)
        }
        tree
      case head :: tail =>
        val matchingNode = newTreeNode.children.find(_.data == head.data)
        if (matchingNode.isEmpty) {
          val node = newTreeNode.addChild(head.data)
          evaluateNodes(node, tail)
        } else {
          evaluateNodes(matchingNode.get, tail)
        }
    }
    evaluateNodes(tree.root, traversal)
  }

  def trimTree(tree: Tree[String], maxLength: Int): Tree[String] = {
    def trimNode(currentNode: Node[String], length: Int): Node[String] = currentNode.children match {
      case Nil =>
        if (length == 0) {
          println("Found end node: " + currentNode.data)
          currentNode
        } else {
          println("Found bad end node: " + currentNode.data)
          null
        }
      case children =>
        currentNode.children = children.map(child => trimNode(child, length - 1)).filter { e =>
          if (e == null) println("Found a null node.")
          e != null
        }
        if (children.nonEmpty) {
          currentNode
        } else {
          null
        }
    }

    trimNode(tree.root, maxLength - 1)
    tree
  }

  def traverseTree(tree: Tree[String]): Unit = {
    val traversal = traverseNodes(tree.root, Seq.empty)
    println(traversal.map(_.data).mkString("->"))
  }

  def traverseNodes(node: Node[String], seq: Seq[Node[String]]): Seq[Node[String]] = node.children match {
    case Nil =>
      println("Reached base of tree " + node.data)

      seq ++ Seq(node)
    case head :: Nil =>
      println("Only one child, chose: " + head.data)

      traverseNodes(head, seq ++ Seq(node))
    case children =>
      println("Choose one: ")

      children foreach (e => println(": " + e.data))
      val choice = StdIn.readInt()
      traverseNodes(children(choice), seq ++ Seq(node))
  }

  def gimmeDatTree(): Tree[String] = {
    val tr = new Tree[String]("root")
    tr.root.addChild("left")
    tr.root.addChild("too short")
    tr.root.children.head.addChild("correct length")
    tr.root.children.head.addChild("also correct length")
    tr.root.children.head.addChild("too long")
    tr.root.children.head.children(2).addChild("branch too long")
    tr
  }

  def getValidBranchTraversals(tree: Tree[String], maxLength: Int): Seq[BranchTraversal[String]] = {
    def evaluateNodes(currentNode: Node[String], sofar: Seq[Node[String]], length: Int):
    Seq[BranchTraversal[String]] = currentNode.children match {
      case Nil =>
        if (sofar.length == length) {
          Seq(sofar)
        } else {
          Seq.empty
        }
      case children =>
        children.flatMap(child => evaluateNodes(child, sofar ++ Seq(child), maxLength))
    }
    evaluateNodes(tree.root, Seq(tree.root), maxLength)
  }

  def printTraversal(traversal: BranchTraversal[String]): Unit = println(traversal.map(_.data).mkString("->"))

}
