package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s_odd = (x:Int) => x % 2 ==1
    val s_even= (x:Int) => x % 2 ==0
    val s_three_multiple= (x:Int) => x % 3 ==0
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("Union Test 1"){
    new TestSets {
      val s = union(s_even, s_odd)
      assert( contains(s, 1), "union(odd,even) should contain 1")
      assert( contains(s, 2), "union(odd,even) should contain 2")
      assert( contains(s, 3), "union(odd,even) should contain 3")
      assert( contains(s, 4), "union(odd,even) should contain 4")
      assert( contains(s, 5), "union(odd,even) should contain 5")
      assert( contains(s, 6), "union(odd,even) should contain 6")
    }
  }

  test("Intersect Test 1") {
    new TestSets {
      val s = intersect(s_even, s_odd)
      assert(!contains(s, 1), "intersect(odd,even) should not contain 1")
      assert(!contains(s, 2), "intersect(odd,even) should not contain 2")
      assert(!contains(s, 3), "intersect(odd,even) should not contain 3")
      assert(!contains(s, 4), "intersect(odd,even) should not contain 4")
      assert(!contains(s, 5), "intersect(odd,even) should not contain 5")
      assert(!contains(s, 6), "intersect(odd,even) should not contain 6")
    }
  }

  test("Intersect Test 2") {
    new TestSets {
      val s = intersect(s_even, s_three_multiple)
      assert(!contains(s, 1), "intersect(even,three multiple) should not contain 1")
      assert(!contains(s, 2), "intersect(even,three multiple) should not contain 2")
      assert(!contains(s, 3), "intersect(even,three multiple) should not contain 3")
      assert(!contains(s, 4), "intersect(even,three multiple) should not contain 4")
      assert(!contains(s, 5), "intersect(even,three multiple) should not contain 5")
      assert(contains(s, 6), "intersect(even,three multiple) should contain 6")
      assert(contains(s, 12), "intersect(even,three multiple) should contain 12")
    }
  }

  test("Diff Test 1") {
    new TestSets {
      val s = diff(s_even, s_three_multiple)
      assert(contains(s, 1), "diff(even,three multiple) should contain 1")
      assert(contains(s, 2), "diff(even,three multiple) should contain 2")
      assert(contains(s, 3), "diff(even,three multiple) should contain 3")
      assert(contains(s, 4), "diff(even,three multiple) should contain 4")
      assert(contains(s, 5), "diff(even,three multiple) should contain 5")
      assert(!contains(s, 6), "diff(even,three multiple) should not contain 6")
      assert(!contains(s, 12), "diff(even,three multiple) should not contain 12")
    }
  }

  test("filter Test 1") {
    new TestSets {
      def p = (x:Int) => x < 10
      val s = filter(s_even, p)
      assert(!contains(s, 1), "filter(even,less than 10) should not contain 1")
      assert(contains(s, 2), "filter(even,less than 10) should contain 2")
      assert(!contains(s, 3), "filter(even,less than 10) should not contain 3")
      assert(contains(s, 4), "filter(even,less than 10) should contain 4")
      assert(!contains(s, 15), "filter(even,less than 10) should not contain 6")
      assert(!contains(s, 10), "filter(even,less than 10) should not contain 12")
    }
  }

  test("Map works") {
    new TestSets {
      def f (x:Int) = 2*x
      def f_plus_one (x:Int) = x+1
      def f_const (x:Int) = 999
      assert(!(map(s1, f))(1), "Set of 1")
      assert((map(s1, f))(2), "Set of 2")
      assert(!(map(s1,f_const))(888), "888 is not 999")
      assert((map(s1,f_const))(999)===true)
      assert((map(s_even,f_plus_one))(5)===true, "map(s_even, f_plus_one) should contain 5")
      assert((map(s_even,f_plus_one))(6)===false, "map(s_even, f_plus_one) should not contain 6")
    }
  }
}
