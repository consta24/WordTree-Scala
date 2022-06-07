import Main._



class WTreeTest extends munit.FunSuite {

  test("split: Simple split test"){
    val s = 'T' :: 'h' :: 'i' :: 's' :: ' ' :: 'i' :: 's' :: ' ' :: 'a' :: ' ' :: 't' :: 'e' :: 's' :: 't' :: Nil
    assert(split(s) == List(List('T','h','i','s'), List('i', 's'), List('a'), List('t','e','s','t')))
  }

  test("split: Making sure trailing whitespace is handled correctly"){
    val s = List('T','e','s','t',' ')
    assert(split(s) == List(List('T','e','s','t')))
  }

  test("split: multiple consecutive whitespaces"){
    val s = List(' ', ' ', ' ')
    assert(split(s) == Nil)
  }

  test("computeTokens: Simple test"){
    val l = List("a","b","c")
    val r = Set(new Token("a",1), new Token("b",1), new Token("c",1))
    assert(computeTokens(l).toSet == r)
  }

  test("computeTokens: Multiple occurrences"){
    val l = List("ba", "ma", "ta", "ma", "ba", "ma")
    val r = Set(new Token("ma",3), new Token("ta",1), new Token("ba",2))
    assert(computeTokens(l).toSet == r)

  }

  test("makeTree: nonempty"){
    val l = List("ma", "ba", "ta", "ma", "ba", "ma")
    val t = Node(Token("ba",2), Node(Token("ta",1), Empty, Empty), Node(Token("ma",3),Empty,Empty))
    assert(makeTree("ba ma ta ma ba ma") == t)
  }
  test("makeTree: empty tree"){
    assert(makeTree("  ") == Empty)
  }

  test("size: empty tree"){
    assert(makeTree(" ").size == 0)
  }

  test("size: nonempty"){
    val l = List("ma", "ba", "ta", "ma", "ba", "ma")
    assert(makeTree("ba ma ta ma ba ma").size == 3)
  }

  test("contains: empty tree"){
    assert(!makeTree(" ").contains("text"))
  }

  test("contains: non-empty tree"){
    assert(makeTree("ba ma ta ma ba ma").contains("ta"))
    assert(makeTree("ba ma ta ma ba ma").contains("ma"))
    assert(makeTree("ba ma ta ma ba ma").contains("ba"))
  }

  test("filter: Simple test 1"){
    assert(makeTree("ba ma ta ma ba ma").filter(_.word == "ma").contains("ma"))
  }

  test("filter: Simple test 2"){
    assert(!makeTree("ba ma ta ma ba ma").filter(_.word != "ma").contains("ma"))
  }

  test("filter: Simple test with empty tree after filtering"){
    assert(!makeTree("ba ma ta ma ba ma").filter(_ => false).contains("ma"))
  }

  test("Scala keyword frequency"){
    assert(scalaFreq == 11)
  }

  test("How many programming languages"){
    assert(progLang == 8)
  }

  test("How many words"){
    assert(wordCount == 139)
  }

}

