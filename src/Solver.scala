
import scala.collection.immutable.HashMap
import scala.concurrent.duration.Duration
import scala.io.{StdIn, Source}
import scala.util.Try
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by Rick on 11/23/15.
  */
object Solver {
  type Board = Seq[Seq[Char]]
  type Position = (Int, Int)
  type WordSeq = (String, Seq[Position])
  type WordBoardPair = (WordSeq, Board)
  type BranchTraversal[A] = Seq[Node[A]]

  val dictionary = loadDictionary()

  def main(args: Array[String]): Unit = {
    val inputted = Try {
      input()
    }.toOption

    if (inputted.isEmpty) {
      println("Failed reading input.")
      System.exit(0)
    }

    val board = inputted.get._1
    val spec = inputted.get._2

    printBoard(board)

    val startTime = System.currentTimeMillis()

    val treedSpecs = spec.map { case (idx, length) =>
      Future {
        val base = expandSpec(board, (idx, length)).map(e => new Tree(e))
        base.map { tree =>
          val otherSpecs = spec.filterNot(_._1 == idx)
          expandFromNode(tree.root, otherSpecs)
          tree
        }
      }
    }

    val evaluatedSpecs = treedSpecs.map(fut => Await.result(fut, Duration.Inf))

    val trimmedTrees = evaluatedSpecs.flatMap(trees => trees.map { tree =>
      trimTree(tree, spec.length)
    }).filter(_.root.children.nonEmpty)

    val endTime = System.currentTimeMillis()

    println(s"Finished in ${endTime - startTime}ms")

    evaluatedSpecs foreach (e => e.flatMap (b => getValidBranchTraversals(b, spec)) foreach (e => println (e.map(_.data._1._1).mkString("->"))))

    queryUser(trimmedTrees)
  }

  def queryUser(validTrees: Seq[Tree[WordBoardPair]]) {
    def getChoice(prompt: String, options: Seq[Node[WordBoardPair]]): Seq[Node[WordBoardPair]] = {
      var opt = Option.empty[Seq[Node[WordBoardPair]]]
      while (opt.isEmpty) {
        opt = Try {
          println(prompt)
          var i = -1
          options foreach { e =>
            i += 1
            println(s" $i: " + e.data._1._1)
          }
          val choice = StdIn.readLine("> ").toInt
          val strMatch = getNodeWord(options(choice))
          options.filter(e => getNodeWord(e) == strMatch)
        }.toOption
      }
      opt.get
    }

    def evaluateLevel(choices: Seq[Node[WordBoardPair]], sofar: Seq[WordBoardPair]): Seq[WordBoardPair] = choices match {
      case Nil =>
        sofar
      case head :: Nil =>
        println("Only one choice - selected " + getNodeWord(head))
        evaluateLevel(head.children, sofar ++ choices.map(_.data))
      case _ =>
        val choice = getChoice("Choose next word: ", choices)
        evaluateLevel(choice.flatMap(_.children).distinct, sofar ++ Seq(choice.head.data))
    }

    val seq = evaluateLevel(validTrees.map(_.root), Seq.empty[WordBoardPair])

    println("Full solution: " + seq.map(_._1._1).mkString(" -> "))
  }

  def trimTree(tree: Tree[WordBoardPair], branchLength: Int): Tree[WordBoardPair] = {
    def trimNode(currentNode: Node[WordBoardPair], length: Int): Node[WordBoardPair] = currentNode.children match {
      case Nil =>
        if (length == 0) {
          println("Found end node: " + currentNode.data._1._1)
          currentNode
        } else {
          null
        }
      case children =>
        currentNode.children = children.map(child => trimNode(child, length - 1)).filter(_ != null)
        currentNode
    }

    trimNode(tree.root, branchLength - 1)
    tree
  }

  def getValidBranchTraversals(tree: Tree[WordBoardPair], okspecs: Seq[(Int, Int)]) = {
    def evaluateNodes(currentNode: Node[WordBoardPair], sofar: Seq[Node[WordBoardPair]], length: Int):
        Seq[BranchTraversal[WordBoardPair]] = currentNode.children match {
      case Nil =>
        if (sofar.length == length) {
          Seq(sofar)
        } else {
          Seq.empty
        }
      case children =>
        children.flatMap(child => evaluateNodes(child, sofar ++ Seq(child), okspecs.length))
    }
    evaluateNodes(tree.root, Seq(tree.root), okspecs.length)
  }

  def expandFromNode(node: Node[WordBoardPair], wordSpecs: Seq[(Int, Int)]): Node[WordBoardPair] = wordSpecs match {
    case head :: Nil =>
      expandSpec(node.data._2, head).foreach { e => node.addChild(e)}
      node
    case head :: tail =>
      expandSpec(node.data._2, head).foreach { e =>
        node.addChild(e)
      }
      node.children foreach (e => expandFromNode(e, tail))
      node
  }

  def expandSpec(board: Board, wordSpec: (Int, Int)): Seq[WordBoardPair] = {
    val range = board.indices
    val perms = permutationsInBoard(board, wordSpec._2, range)
    val filtered = perms.filter { case (word, seq) =>
      val use = if (word.last == 's') {
        word.dropRight(1)
      } else {
        word
      }
      val ret = dictionary.contains(use.hashCode) || dictionary.contains(word.hashCode)
      if (ret) {
        val w = dictionary.getOrElse(use.hashCode, dictionary.get(word.hashCode).get)
        w == word || w == use
      } else {
        false
      }
    }
    filtered.map(wordseq => wordseq -> settleBoard(board, wordseq))
  }

  def input(): (Board, Seq[(Int, Int)]) = {
    val size = StdIn.readLine("What is the width/height of the board: ").toInt
    println("Input the board, line by line, with nothing between the characters:")
    val board = for(r <- 0 until size) yield StdIn.readLine(": ").split("").map(_.head).toSeq
    var i = 0
    val words = StdIn.readLine("Now input the length of words to find, separated by spaces: ").split(" ").map { e =>
      val tup = (i, e.toInt)
      i += 1
      tup
    }
    i = 0
    val newSeq = Seq.fill(words.length)(0).map { a =>
      val n = words(i)
      i += 1
      n
    }
    (board, newSeq)
  }


  def settleBoard(originalBoard: Board, wordSeq: WordSeq): Board = {
    var newBoard = originalBoard
    wordSeq._2.foreach(position => newBoard = newBoard.updated(position._1, newBoard(position._1).updated(position._2, '-')))
    val limit = newBoard.indices.last

    def settleColumn(column: Position) = {
      for (height <- 0 to limit) {
        if (letterAtPosition(newBoard, (column._1 - height, column._2)).isEmpty) {
          val abovePosition = (column._1 - height - 1, column._2)
          newBoard = updatePosition(letterAtPosition(newBoard, abovePosition).getOrElse('-'), newBoard, (column._1 - height, column._2))
          newBoard = updatePosition('-', newBoard, abovePosition)
        }
      }
    }

    for (repeat <- 0 to limit) {
      for (width <- 0 to limit) {
        settleColumn((newBoard.length - 1, width))
      }
    }

    newBoard
  }

  def updatePosition(newVal: Char, board: Board, position: Position): Board = {
    if (position._1 >= 0 && position._2 >= 0) {
      board.updated(position._1, board(position._1).updated(position._2, newVal))
    } else {
      board
    }
  }

  def permutationsInBoard(board: Board, wordLength: Int, range: Range) = range.flatMap(x => range.flatMap { y =>
    startSearch(board, (y, x), wordLength)
  })

  def startSearch(board: Board, position: Position, length: Int): List[WordSeq] = {
    def findWords(word: String, board: Board, innerPos: Position, checked: Seq[Position], length: Int): List[WordSeq] = length match {
      case l if l == 0 =>
        List((word, checked))
      case l =>
        val chars = adjacentCharacters(board, innerPos)
        val words = chars.filterNot(c => checked.contains(c._2)).flatMap { case (char, pos) =>
          findWords(word + char, board, pos, checked :+ pos, l - 1)
        }
        words
    }

    letterAtPosition(board, position) match {
      case Some(char) =>
        val words = findWords(char.toString, board, position, Seq(position), length - 1)
        words
      case None =>
        List.empty[WordSeq]
    }
  }

  def adjacentCharacters(board: Board, position: Position): List[(Char, Position)] = {
    val range = List(-1, 0, 1)
    range.flatMap(y => range.map(x => (y + position._1, x + position._2)))
      .flatMap { p =>
        letterAtPosition(board, p) match {
          case Some(c) => Option(c -> p)
          case _ => None
        }
      }
  }

  def loadDictionary(path: String = "/usr/share/dict/words"): HashMap[Int, String] = {
    var map = new HashMap[Int, String]
    Source.fromFile(path).getLines().foreach { word =>
      val w = word.toLowerCase
      map += w.hashCode -> w
    }
    map += "box".hashCode -> "box"
    map
  }

  def letterAtPosition(board: Board, position: Position): Option[Char] = Try{
    board(position._1)(position._2) match {
      case '-' => throw new Exception
      case o => o
    }
  }.toOption

  def getNodeWord(node: Node[WordBoardPair]): String = node.data._1._1

  def printBoard(board: Board) = board.foreach(r => println(r.mkString(" ")))
}


object PrefabBoards {
  val fanCandle = (
    toBoard(Seq(
      "enf",
      "lac",
      "dna"
    )), Seq((0, 3), (1, 6))
  )

  val tableRecordNorth = (
    toBoard(Seq(
      "enrd",
      "loco",
      "hbat",
      "rtre"
    )), Seq((0,5), (1,6), (2,5))
  )

  val hardBitch = (
    toBoard(Seq (
      "fpot",
      "pann",
      "ilee",
      "lucr"
    )), Seq((0, 5), (1,4), (2, 7))
  )

  val lotsOfBads = (
    toBoard(Seq(
      "fhfs",
      "lsik",
      "arce",
      "gatn"
    )), Seq((0,4), (1,4), (2,8))
  )

  def toBoard(arr: Seq[String]): Solver.Board = arr.map(_.toCharArray.toSeq)
}

class Tree[A] (in: A) {
  val root = Node(in, Seq.empty)

  override def toString: String = s"${root.toString} -> " + root.children.mkString(",")
}

case class Node[A](data: A, var children: Seq[Node[A]]) {
  def addChild(data: A, childNodes: Seq[Node[A]] = Seq.empty): Seq[Node[A]] = {
    children = children ++ List(Node(data, childNodes))
    children
  }
}
