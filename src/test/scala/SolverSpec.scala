import java.nio.file.{Paths, Files}

import com.bau5.wordbrainsolver.{Solver, Tree}
import com.typesafe.config.ConfigFactory
import org.scalatest.{Matchers, FlatSpec}


/**
  * Created by Rick on 12/10/15.
  */
class SolverSpec extends FlatSpec with Matchers {
  val conf = ConfigFactory.load()

  "Solver" should "find the dictionary" in {
    fileExists(conf.getString("solver.dictionary-path")) should be (true)
  }

  it should "find the input path" in {
    fileExists(conf.getString("solver.input-path")) should be (true)
  }

  "The tree operation" should "find branch traversals of correct length" in {
    val tree = getTree()
    val traversals = Solver.getValidBranchTraversals(tree, 3)
    traversals forall(_.length == 3) should be (true)
  }

  def fileExists(path: String): Boolean = {
    Files.exists(Paths.get(path))
  }

  def getTree(): Tree[String] = {
    val tr = new Tree[String]("root")
    tr.root.addChild("left")
    tr.root.addChild("too short")
    tr.root.children.head.addChild("correct length")
    tr.root.children.head.addChild("also correct length")
    tr.root.children.head.addChild("too long")
    tr.root.children.head.children(2).addChild("branch too long")
    tr
  }
}
