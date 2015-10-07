package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(4,11)), "4,11")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
      assert(goal == Pos(4,7))
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

  /*
  trait LevelBug extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooo-------
      |oS-ooo----
      |oo-oooooo-
      |-o-ooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = Nil
  }
test( "solution for levelBug") {
    new LevelBug {
      assert((solution) == Nil)
      assert(solve(solution) == Block(goal, goal))
    }
  }

*/

  trait Level3_1 extends SolutionChecker {
    /* terrain for level 1*/
    val level =
      """oooooooooo
      |oooooooooo
      |ooooSooToo
      |oooooooooo
      |oooooooooo
      |oooooooooo""".stripMargin

    val optsolution = List(Right, Right)
  }

  trait Level3_2 extends SolutionChecker {
    /* terrain for level 1*/
    val level =
      """oooooooooo
        |oooooooooo
        |oTooSooooo
        |oooooooooo
        |oooooooooo
        |oooooooooo""".stripMargin

    val optsolution = List(Left, Left)
  }

  trait Level3_3 extends SolutionChecker {
    /* terrain for level 1*/
    val level =
      """oooooooooo
        |oooooooooo
        |ooooSooooo
        |oooooooooo
        |oooooooooo
        |ooooTooooo""".stripMargin

    val optsolution = List(Down, Down)
  }

  test( "solution for Level3") {
    new Level3_1{
      assert(solve(solution) === Block(goal, goal))
      assert(solution.length === 2)
      assert(solution === optsolution)
    }
    new Level3_2{
      assert(solve(solution) === Block(goal, goal))
      assert(solution.length === 2)
      assert(solution === optsolution)
    }
    new Level3_3{
      assert(solve(solution) === Block(goal, goal))
      assert(solution.length === 2)
      assert(solution === optsolution)
    }
  }

}
