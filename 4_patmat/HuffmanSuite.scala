package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("Frech encode"){
    println(decodedSecret)
    val cloverAnswer =  List( 'h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l' )
    assert(decodedSecret === cloverAnswer)
  }

  val pintosString = "2.1.3 Synchronization Proper synchronization is an important part of the solutions to these problems. Any synchronization problem can be easily solved by turning interrupts off: while interrupts are off, there is no concurrency, so theres no possibility for race conditions. Therefore, its tempting to solve all synchronization problems this way, but dont. Instead, use semaphores, locks, and condition variables to solve the bulk of your synchronization problems. Read the tour section on synchronization (see Section A.3 [Synchronization], page 66) or the comments in threads/synch. if youre unsure what synchronization primitives may be used in what situations.  In the Pintos projects, the only class of problem best solved by disabling interrupts is coordinating data shared between a kernel thread and an interrupt handler. Because interrupt handlers cant sleep, they cant acquire locks. This means that data shared between kernel threads and an interrupt handler must be protected within a kernel thread by turning off interrupts.  This project only requires accessing a little bit of thread state from interrupt handlers.  For the alarm clock, the timer interrupt needs to wake up sleeping threads. In the advanced scheduler, the timer interrupt needs to access a few global and per-thread variables. When you access these variables from kernel threads, you will need to disable interrupts to prevent the timer interrupt from interfering.  When you do turn off interrupts, take care to do so for the least amount of code possible, or you can end up losing important things such as timer ticks or input events. Turning off interrupts also increases the interrupt handling latency, which"

  test("Quick Encode Test"){
    val charList1 = "ab".toList
    val charList2 = "aabababababaaabab".toList
    new TestTrees {
      assert(encode(t1)(charList1) === quickEncode(t1)(charList1))
      assert(encode(t1)(charList2) === quickEncode(t1)(charList2))
    }
  }

  test("Quick Encode Test2"){
    val charList1 = string2Chars(pintosString)
    val tree = createCodeTree(charList1)
    val query = "synch".toList
    val cloverAnswer =  List(0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0)
    assert(encode(tree)(query) === quickEncode(tree)(query))
    assert(encode(tree)(query) === cloverAnswer )
  }


  test("Creation Test"){
    val charList1 = string2Chars(pintosString)
    val charList2 = string2Chars("Synchronization")
    val tree = createCodeTree(charList1)
    val cloverAnswer = List(0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1)
    val encoded = encode(tree)(charList2)
    assert(encoded === cloverAnswer )
    assert(decode(tree, encoded) === charList2)
  }
}
