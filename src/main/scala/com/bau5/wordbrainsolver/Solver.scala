package com.bau5.wordbrainsolver

import com.typesafe.config.ConfigFactory

import scala.collection.immutable.HashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.io.{Source, StdIn}
import scala.util.Try

/**
  * Created by Rick on 11/23/15.
  */
object Solver {
  type Board = Seq[Seq[Char]]
  type Position = (Int, Int)
  type WordSeq = (String, Seq[Position])
  type WordBoardPair = (WordSeq, Board)
  type BranchTraversal[A] = Seq[Node[A]]

  val conf = ConfigFactory.load()
  val dictionary = loadDictionary()

  def main(args: Array[String]): Unit = {
    val reader = new ImageReader

    val prompt = conf.getString("solver.input-path")
    val fileName = StdIn.readLine("Enter the file name \n" + prompt)

    val readerOutput = Option(reader.processScreenshot(conf, prompt + fileName))
    if (readerOutput.isEmpty) {
      sys.error("No input was received from ImageReader...")
      reader.post()
      System.exit(0)
    }
    val boardProps = readerOutput.get

//    val inputted = input()                // Get the input from the user
    val inputted = Option(getInputFromReader(boardProps))

    if (inputted.isEmpty) {
      println("Failed reading input.")    // exit if there is an error
      reader.post()
      System.exit(0)
    }

    val board = inputted.get._1
    val spec = inputted.get._2

    println("Successfully read the board.")
    printBoard(board)                     // display the board.
    println("Words to find: " + spec.map(_._2).mkString(", "))

    val startTime = System.currentTimeMillis()

    // Parallelize the evaluation of the base specs.
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

    // Get the results, find the valid traversals, group them by their initial node and rebuild new trees
    val evaluatedSpecs = treedSpecs.map(fut => Await.result(fut, Duration.Inf))
    val traversals = evaluatedSpecs.flatMap { trees  =>
      trees.flatMap(tree => getValidBranchTraversals(tree, spec.length))
    }
    val grouped = traversals.groupBy(_.head.data)
    val rebuilt = grouped.map { case (root, rest) =>
      val newTree = new Tree(root)
      rest.foreach(traversal => buildTree(newTree, traversal.tail))
      newTree
    }
    val endTime = System.currentTimeMillis()

    println(s"Finished in ${endTime - startTime}ms")

    // Interact with the user to have them enter the correct answer.
    queryUser(rebuilt.toSeq)
    reader.post();
  }

  def getInputFromReader(props: BoardProperties): (Board, Seq[(Int, Int)]) = {
    val board = props.getOutput.replace(" ", "")
      .split("\n")
      .map(_.toCharArray.toSeq)
      .toSeq
    var i = 0
    val newSeq = Seq.fill(props.boxes.length)(0).map { a =>
      val n = props.boxes(i)
      val idx = i
      i += 1
      (idx, n.toInt)
    }
    (board, newSeq)
  }

  /**
    *   Enters interaction mode with the user. Allows them to select
    * the order of entry and only the correct traversal.
    *
    * @param validTrees all trees with valid traversals to solve the puzzle.
    */
  def queryUser(validTrees: Seq[Tree[WordBoardPair]]) {
    def getChoice(prompt: String, options: Seq[Node[WordBoardPair]]): Seq[Node[WordBoardPair]] = {
      var opt = Option.empty[Seq[Node[WordBoardPair]]]
      while (opt.isEmpty) {
        opt = Try {
          println(prompt)
          var i = 0
          options.foreach { e =>
            i += 1
            println(s" $i: " + e.data._1._1)
          }
          val choice = StdIn.readLine("> ").toInt
          val strMatch = getNodeWord(options(choice - 1))
          options.filter(node => getNodeWord(node) == strMatch)
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
        val choice = getChoice( { if (sofar.isEmpty) { "Choose frist word"} else { "Choose next word: " } }, choices)
        evaluateLevel(choice.flatMap(_.children).distinct, sofar ++ Seq(choice.head.data))
    }

    val seq = evaluateLevel(validTrees.map(_.root), Seq.empty[WordBoardPair])

    println("Full solution: " + seq.map(_._1._1).mkString(" -> "))
  }

  /**
    *   Takes the tree, which may not necessarily contain only valid traversals,
    * and extracts all the traversals that have only the correct length, that is the
    * traversals that are exactly the length of the number of words to find.
    *
    * @param tree the tree to evaluate
    * @param maxLength the length of the branches we want to find
    * @return
    */
  def getValidBranchTraversals[A](tree: Tree[A], maxLength: Int) = {
    def evaluateNodes(currentNode: Node[A], sofar: Seq[Node[A]]):
        Seq[BranchTraversal[A]] = currentNode.children match {
      case Nil =>
        // We've found a leaf, return this traversal if the length of it is equal
        //to the length that we're searching for
        if (sofar.length == maxLength) {
          Seq(sofar)
        } else {
          Seq.empty
        }
      case children =>
        children.flatMap(child => evaluateNodes(child, sofar ++ Seq(child)))
    }
    evaluateNodes(tree.root, Seq(tree.root))
  }

  /**
    *   Takes a tree and splices a traversal into it. It will not duplicate like nodes.
    *
    * @param tree the tree to splice into
    * @param traversal the traversal to splice into the tree
    * @return the tree with the new traversal within it
    */
  def buildTree(tree: Tree[WordBoardPair], traversal: BranchTraversal[WordBoardPair]): Tree[WordBoardPair] = {
    def evaluateNodes(newTreeNode: Node[WordBoardPair], trav: BranchTraversal[WordBoardPair]): Tree[WordBoardPair] = trav match {
      case head :: Nil =>
        // We're at the end of the traversal. Add this leaf to the tree if it doesn't already exist.
        if (newTreeNode.children.forall(_.data != head.data)) {
          newTreeNode.addChild(head.data)
        }
        tree
      case head :: tail =>
        // Try to find a matching node so as to not add a duplicate, otherwise add it and continue.
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

  /**
    *  For a node and remaining word specs, evaluate the possible words that could
    * be found on the board and add them as children to this node
    *
    * @param node the current node being evaluated
    * @param wordSpecs the remaining specs that are valid
    * @return the node with it's children populated
    */
  def expandFromNode(node: Node[WordBoardPair], wordSpecs: Seq[(Int, Int)]): Node[WordBoardPair] = wordSpecs match {
    case head :: Nil =>
      expandSpec(node.data._2, head).foreach(e => node.addChild(e))
      node
    case head :: tail =>
      expandSpec(node.data._2, head).foreach(e => node.addChild(e))
      // For the new children to the node, continue building the tree.
      node.children.foreach(e => expandFromNode(e, tail))
      node
  }

  /**
    *  Takes the board and specification and returns all the possible dictionary
    * words that can be formed for this spec.
    *
    * @param board the board to evaluate
    * @param wordSpec the word specification to evaluate
    * @return List of 2 tuples of the valid, dictionary checked word sequence
    *         and the settled board after that word has been removed.
    */
  def expandSpec(board: Board, wordSpec: (Int, Int)): Seq[WordBoardPair] = {
    val range = board.indices

    // Find all permutations in the board of this length
    val perms = permutationsInBoard(board, wordSpec._2, range)

    // Filter out the non-dictionary words
    val filtered = perms.filter { case (word, seq) =>
      // Plurals are not included in this dictionary, this is a simple work
      //around, but may not be complete.
      val use = if (word.last == 's') {
        word.dropRight(1)
      } else {
        word
      }

      // See if the word is in the dictionary
      val ret = dictionary.contains(use.hashCode) || dictionary.contains(word.hashCode)
      if (ret) {
        // Double check that it is the correct word, was experience hash collisions.
        val w = dictionary.getOrElse(use.hashCode, dictionary.get(word.hashCode).get)
        w == word || w == use
      } else {
        false
      }
    }
    filtered.map(wordseq => wordseq -> settleBoard(board, wordseq))
  }

  /**
    *  Allows the user to input the specifications of the board. Wrapped in a scala.util.Try
    * to catch errors of input.
    *
    * @return an optional 2 tuple of Board and the word specifications (word lengths to find).
    *         Returns None if there was an error during the input process.
    */
  def input(): Option[(Board, Seq[(Int, Int)])] = Try {
    val size = StdIn.readLine("What is the width/height of the board: ").toInt
    println("Input the board, line by line, with nothing between the characters:")
    val board = for(r <- 0 until size) yield StdIn.readLine(": ").split("").map(_.head).toSeq
    var i = 0

    // The word specifications are indexed to allow for proper filtering in another
    //stage of the program.
    val words = StdIn.readLine("Now input the length of words to find, separated by spaces: ").split(" ").map { e =>
      val tup = (i, e.toInt)
      i += 1
      tup
    }
    i = 0

    // Work around for a matching error that was happening for WrappedArrays. This
    //forces it to a Sequence, where .toSeq was not casting it fully.
    val newSeq = Seq.fill(words.length)(0).map { a =>
      val n = words(i)
      i += 1
      n
    }
    (board, newSeq)
  }.toOption


  /**
    *  Removes a word from the board and settles the columns.
    *
    * @param originalBoard the board before removal
    * @param wordSeq the sequence sensitive word to remove
    * @return the new board with the word removed and the columns settled
    */
  def settleBoard(originalBoard: Board, wordSeq: WordSeq): Board = {
    // Take the original board and remove this word sequence from it
    var newBoard = originalBoard
    wordSeq._2.foreach { position =>
      newBoard = newBoard.updated(position._1, newBoard(position._1).updated(position._2, '-'))
    }
    val limit = newBoard.indices.last

    // For each column, move all letters down if there is a '-' in it
    def settleColumn(column: Position) = {
      for (height <- 0 to limit) {
        if (letterAtPosition(newBoard, (column._1 - height, column._2)).isEmpty) {
          val abovePosition = (column._1 - height - 1, column._2)
          newBoard = updatePosition(letterAtPosition(newBoard, abovePosition).getOrElse('-'),
            newBoard, (column._1 - height, column._2))
          newBoard = updatePosition('-', newBoard, abovePosition)
        }
      }
    }

    // Now that the word is removed, settle the columns.
    for (repeat <- 0 to limit) {
      for (width <- 0 to limit) {
        settleColumn((newBoard.length - 1, width))
      }
    }

    newBoard
  }

  /**
    *  Helper function to update the character at a position on the board.
    *
    * @param newVal the new character
    * @param board the board to be updated
    * @param position the position to update
    * @return the updated board
    */
  def updatePosition(newVal: Char, board: Board, position: Position): Board = {
    if (position._1 >= 0 && position._2 >= 0) {
      board.updated(position._1, board(position._1).updated(position._2, newVal))
    } else {
      board
    }
  }

  /**
    *  Helper function to find all permutations on the board of a certain length.
    *
    * @param board the board to use
    * @param wordLength the length of the word
    * @param range the size of the board
    * @return list of all possible words of the correct length starting from every position
    *         on the board. These are not necessarily dictionary words.
    */
  def permutationsInBoard(board: Board, wordLength: Int, range: Range): Seq[WordSeq] =
    range.flatMap(x => range.flatMap { y =>
      startSearch(board, (y, x), wordLength)
    })

  /**
    *  The driver function for evaluating the possible words on a board starting from
    * a given position of the given length.
    *
    * @param board the board to perform the lookup on
    * @param position the position to begin building words from
    * @param length the exact length of the word
    * @return a list of WordSeq (a word and the moves taken to build the word). This
    *         list will contain "words" that are not dictionary words.
    */
  def startSearch(board: Board, position: Position, length: Int): Seq[WordSeq] = {
    def findWords(word: String, board: Board, innerPos: Position, checked: Seq[Position], length: Int):
        Seq[WordSeq] = length match {
      case l if l == 0 =>
        // We are at the end of the allowed length, return this sequences of characters
        List((word, checked))
      case l =>
        // Grab all adjacent characters, and build words recursively using each of them
        val chars = adjacentCharacters(board, innerPos)
        val words = chars.filterNot(c => checked.contains(c._2))
          .flatMap { case (char, pos) =>
            findWords(word + char, board, pos, checked :+ pos, l - 1)
          }
        words
    }

    letterAtPosition(board, position) match {
      case Some(char) =>
        findWords(char.toString, board, position, Seq(position), length - 1)
      case None =>
        Seq.empty[WordSeq]
    }
  }

  /**
    *  Returns a list of adjacent characters to a position on the board. This searches
    * the 3x3 grid around the position, grabbing all non '-' characters.
    *
    * @param board the board to lookup on
    * @param position the position to find characters around
    * @return list of 2 tuples of the character and it's position that are adjacent
    */
  def adjacentCharacters(board: Board, position: Position): List[(Char, Position)] = {
    val range = List(-1, 0, 1)
    range.flatMap { y =>
      range.map(x => (y + position._1, x + position._2))
    }.flatMap { p =>
      letterAtPosition(board, p) match {
        case Some(c) => Option(c -> p)
        case _ => None
      }
    }
  }

  /**
    *  Loads the dictionary of words to use at the path in the config.
    *
    * @return the dictionary represented as a HashMap
    */
  def loadDictionary(): HashMap[Int, String] = {
    val path = conf.getString("solver.dictionary-path")
    var map = new HashMap[Int, String]
    // For each line in the dictionary, add it to the HashMap with the string's
    //hash used as the key
    Source.fromFile(path).getLines().foreach { word =>
      if (word.head.isLower) {
        val w = word.toLowerCase
        map += w.hashCode -> w
      }
    }
    // Add some words that are missing from the dictionary... (maybe I should find a different one)
    map += "box".hashCode -> "box"
    map += "tv".hashCode -> "tv"
    map += "barbell".hashCode -> "barbell"
    map
  }

  /**
    *  Gets the letter at the position on the given board. Wrapped in a scala.util.Try
    * to allow for out of bound exceptions.
    *
    * @param board the board to lookup on
    * @param position the position to lookup
    * @return Optional char, where None represents either off the board
    *         or the there is no character at the position (it is a '-')
    */
  def letterAtPosition(board: Board, position: Position): Option[Char] = Try {
    board(position._1)(position._2) match {
      case '-' => throw new Exception
      case o => o
    }
  }.toOption

  def getNodeWord(node: Node[WordBoardPair]): String = node.data._1._1

  def printBoard(board: Board) = board.foreach(r => println(r.mkString(" ")))
}

/**
  * Contains several predefined boards for use during testing.
  */
object PrefabBoards {
  val fanCandle = (
    toBoard(Seq(
      "enf",
      "lac",
      "dna"
    )), Seq((0, 3), (1, 6))
  )

  val umWhat = (
    toBoard(Seq(
      "wpdp",
      "nhls",
      "mtih",
      "ooee"
    )), Seq((0,4), (1,5), (2,3), (3,4))
  )

  val profitBulletWell = (
    toBoard(Seq(
      "wtep",
      "bltr",
      "ulio",
      "ellf"
    )), Seq((0,6), (1,4), (2,6))
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

case class Node[A](data: A, var children: Seq[Node[A]]) {
  // Helper function to add a child to the node
  def addChild(data: A, childNodes: Seq[Node[A]] = Seq.empty): Node[A] = {
    val newNode = Node(data, childNodes)
    children = children ++ List(newNode)
    newNode
  }
}

class Tree[A] (in: A) {
  val root = Node(in, Seq.empty)

  override def toString: String = s"${root.toString} -> " + root.children.mkString(",")
}
