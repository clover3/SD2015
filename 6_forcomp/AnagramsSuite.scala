package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite {

  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }



  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }

 test("wordOccurrences: zaybwc") {
    assert(wordOccurrences("zaybwc") === List(('a', 1), ('b', 1), ('c', 1), ('w', 1), ('y', 1), ('z', 1)))
  }

  test("sentenceOccurrences: Robert Love") {
    assert(sentenceOccurrences(List("Robert", "Love")) === List(('b', 1), ('e', 2), ('l', 1), ('o', 2), ('r',2), ('t', 1), ('v',1)))
  }

  test("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
  }



  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }



  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }

  test("subtract: abbc - abbc") {
    val abbc = List(('a', 1), ('b', 2), ('c', 1))
    assert(subtract(abbc, abbc) === Nil)
  }

  test("subtract: lard - ra") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 3))
    val r = List(('r', 1), ('a', 1))
    val ld = List(('d', 1), ('l', 1), ('r', 2))
    assert(subtract(lard, r) === ld)
  }


  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
    assert(combinations(List(('a',1))) === List(Nil,List(('a',1))))
  }

  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    assert(combinations(abba).toSet === abbacomb.toSet)
  }



  test("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }

  test("sentence anagrams: Linux"){
    val sentence = List("Linux")
    assert(sentenceAnagrams(sentence)===List(List("Linux")))
  }

  test("sentence anagrams: Many ab"){
    val sentence = List("ab", "ab", "ab", "ab", "ab", "ab", "ab", "ab", "ab", "ab", "ab", "ab", "ab", "ab")
    val anas =  List(List("Abba", "Abba", "Abba", "Abba", "Abba", "Abba", "Abba"))
    assert(sentenceAnagrams(sentence)=== anas)
  }


  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }

  test("sentence anagrams: what the hell") {
    val sentence = List("what", "the", "hell")
    val anas =  List(List("hell", "thaw", "the"), List("hell", "what", "the"), List("hell", "hew", "that"), List("hell", "that", "hew"), List("hell", "the", "thaw"), List("hell", "the", "what"), List("thaw", "hell", "the"), List("what", "hell", "the"), List("thaw", "the", "hell"), List("what", "the", "hell"), List("hew", "hell", "that"), List("hew", "that", "hell"), List("that", "hell", "hew"), List("that", "hew", "hell"), List("the", "hell", "thaw"), List("the", "hell", "what"), List("the", "thaw", "hell"), List("the", "what", "hell"))
    assert(sentenceAnagrams(sentence) === anas)
  }

  test("sentence anagrams: ZZZzzz") {
    val sentence = List("ZZZzzz")
    val anas = Nil
    assert(sentenceAnagrams(sentence) === anas)
  }

test("sentence anagrams: Apple pie") {
  val sentence = List("Apple", "pie")
  val anas = Set(List("ale", "pep", "pi"), List("leap", "pipe"), List("pail", "peep"), List("Al", "pep", "pie"), List("pipe", "plea"), List("pep", "Al", "pie"), List("pie", "Al", "pep"), List("ale", "pi", "pep"), List("pipe", "leap"), List("peep", "pi", "Al"), List("pep", "pi", "ale"), List("Al", "pie", "pep"), List("plea", "pipe"), List("pie", "pep", "Al"), List("peep", "Al", "pi"), List("pep", "pie", "Al"), List("peal", "pipe"), List("pep", "ale", "pi"), List("pie", "apple"), List("pale", "pipe"), List("pipe", "pale"), List("pip", "Peale"), List("pipe", "peal"), List("Al", "peep", "pi"), List("Peale", "pip"), List("pi", "ale", "pep"), List("Al", "pi", "peep"), List("apple", "pie"), List("peep", "pail"), List("pi", "Al", "peep"), List("pi", "pep", "ale"), List("pi", "peep", "Al"))
  assert(sentenceAnagrams(sentence).toSet === anas)
}
}
