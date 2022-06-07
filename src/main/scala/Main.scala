

import scala.annotation.tailrec
import scala.language.postfixOps


abstract class WTree extends WTreeInterface {
  override def filter(pred: Token => Boolean): WTree = {
    filterAux(pred, Empty)
  }
  def filterAux(pred: Token => Boolean, acc: WTree): WTree

}

case object Empty extends WTree {
  override def isEmpty = true
  override def ins(w: Token): WTree = Node(w, Empty, Empty)
  override def filterAux(pred: Token => Boolean, acc: WTree): WTree = acc
  override def size: Int = 0
  override def contains(s: String): Boolean = false
}

case class Node(word: Token, left: WTree, right: WTree) extends WTree {
  override def isEmpty = false

  override def ins(w: Token): WTree =
    if (w.freq > word.freq) Node(word, left, right.ins(w))
    else Node(word, left.ins(w), right)

  override def contains(s: String): Boolean = {
    if(word.word == s)
      true
    else if(left.contains(s))
      true
    else if(right.contains(s))
      true
    else
      false
  }

  override def size: Int = {
    if(left == Empty)
      0
    if(right == Empty)
      0
    1 + left.size + right.size
  }

  def filterAux(pred: Token => Boolean, acc: WTree): WTree = {
    if(pred(word))
      right.filterAux(pred, left.filterAux(pred, acc.ins(word)))
    else
      right.filterAux(pred, left.filterAux(pred, acc))
  }
}


object Main {

  val scalaDescription: String = "Scala is a strong statically typed general-purpose programming language which supports both object-oriented programming and functional programming designed to be concise many of Scala s design decisions are aimed to address criticisms of Java Scala source code can be compiled to Java bytecode and run on a Java virtual machine. Scala provides language interoperability with Java so that libraries written in either language may be referenced directly in Scala or Java code like Java, Scala is object-oriented, and uses a syntax termed curly-brace which is similar to the language C since Scala 3 there is also an option to use the off-side rule to structure blocks and its use is advised martin odersky has said that this turned out to be the most productive change introduced in Scala 3 unlike Java, Scala has many features of functional programming languages like Scheme, Standard ML, and Haskell, including currying, immutability, lazy evaluation, and pattern matching it also has an advanced type system supporting algebraic data types, covariance and contravariance, higher-order types (but not higher-rank types), and anonymous types other features of Scala not present in Java include operator overloading optional parameters named parameters and raw strings conversely a feature of Java not in Scala is checked exceptions which has proved controversial"

  def split(text: List[Char]): List[List[Char]] = {
    @tailrec
    def aux(currentListBeforeToken: List[Char] = List.empty, list: List[Char], acc: List[List[Char]] = List.empty): List[List[Char]] = {
      if (list.isEmpty || (list.tail.isEmpty && list.head == ' ')) {
        currentListBeforeToken.reverse :: acc
      } else {
        list.head match {
          case ' ' => aux(List.empty[Char], list.tail, currentListBeforeToken.reverse :: acc)
          case otherCharacter => aux(otherCharacter :: currentListBeforeToken, list.tail, acc)
        }
      }
    }

    if (text.filter(_ != ' ') != Nil)
      aux(list = text).reverse
    else
      Nil
  }

  def computeTokens(words: List[String]): List[Token] = {
    words.toSet.map(x => new Token(x, words.count(_ == x))).toList
  }

  def tokensToTree(tokens: List[Token]): WTree = {
    val emptyTree: WTree = Empty
    tokens.foldLeft(emptyTree) { (tree: WTree, x: Token) =>
      tree.ins(x)
    }
  }



  def makeTree(s: String): WTree = {
    tokensToTree(computeTokens(split(s.toList).map(x => x.mkString)))
  }

  def wordSet: WTree = {
    makeTree(scalaDescription)
  }


  def scalaFreq: Int = {
    wordSet.filter(_.word == "Scala") match {
      case Node(word, left, right) => word.freq
    }
  }


  def progLang: Int = {
    wordSet.filter(_.word(0).isUpper).size
  }


  def wordCount: Int = {
    split(scalaDescription.toList).filter(_.size > 3).size
  }

}

