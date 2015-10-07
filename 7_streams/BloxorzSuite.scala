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
       assert(solve(solution) == startBlock)
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

  test("stream") { new Level1 {
      assert(from(Stream((Block(Pos(1,1),Pos(1,1)),List())), Set()).toSet === Stream((Block(Pos(1,1),Pos(1,1)),List()), (Block(Pos(2,1),Pos(3,1)),List(Down)), (Block(Pos(1,2),Pos(1,3)),List(Right)), (Block(Pos(1,1),Pos(1,1)),List(Up, Down)), (Block(Pos(2,2),Pos(3,2)),List(Right, Down)), (Block(Pos(2,2),Pos(2,3)),List(Down, Right)), (Block(Pos(1,1),Pos(1,1)),List(Left, Right)), (Block(Pos(1,4),Pos(1,4)),List(Right, Right)), (Block(Pos(1,2),Pos(1,2)),List(Up, Right, Down)), (Block(Pos(2,3),Pos(3,3)),List(Right, Right, Down)), (Block(Pos(3,2),Pos(3,3)),List(Down, Down, Right)), (Block(Pos(2,1),Pos(2,1)),List(Left, Down, Right)), (Block(Pos(2,4),Pos(2,4)),List(Right, Down, Right)), (Block(Pos(2,4),Pos(3,4)),List(Down, Right, Right)), (Block(Pos(1,0),Pos(1,1)),List(Left, Up, Right, Down)), (Block(Pos(1,3),Pos(1,4)),List(Right, Up, Right, Down)), (Block(Pos(1,3),Pos(1,3)),List(Up, Right, Right, Down)), (Block(Pos(3,1),Pos(3,1)),List(Left, Down, Down, Right)), (Block(Pos(3,4),Pos(3,4)),List(Right, Down, Down, Right)), (Block(Pos(0,1),Pos(1,1)),List(Up, Left, Down, Right)), (Block(Pos(2,5),Pos(2,6)),List(Right, Right, Down, Right)), (Block(Pos(2,5),Pos(3,5)),List(Right, Down, Right, Right)), (Block(Pos(0,0),Pos(0,1)),List(Up, Left, Up, Right, Down)), (Block(Pos(2,0),Pos(2,1)),List(Down, Left, Up, Right, Down)), (Block(Pos(2,3),Pos(2,4)),List(Down, Right, Up, Right, Down)), (Block(Pos(1,5),Pos(1,5)),List(Right, Right, Up, Right, Down)), (Block(Pos(1,1),Pos(1,2)),List(Left, Up, Right, Right, Down)), (Block(Pos(1,4),Pos(1,5)),List(Right, Up, Right, Right, Down)), (Block(Pos(1,1),Pos(2,1)),List(Up, Left, Down, Down, Right)), (Block(Pos(1,4),Pos(2,4)),List(Up, Right, Down, Down, Right)), (Block(Pos(3,5),Pos(3,6)),List(Right, Right, Down, Down, Right)), (Block(Pos(0,0),Pos(1,0)),List(Left, Up, Left, Down, Right)), (Block(Pos(0,2),Pos(1,2)),List(Right, Up, Left, Down, Right)), (Block(Pos(3,5),Pos(3,6)),List(Down, Right, Right, Down, Right)), (Block(Pos(2,7),Pos(2,7)),List(Right, Right, Right, Down, Right)), (Block(Pos(1,5),Pos(1,5)),List(Up, Right, Down, Right, Right)), (Block(Pos(4,5),Pos(4,5)),List(Down, Right, Down, Right, Right)), (Block(Pos(2,6),Pos(3,6)),List(Right, Right, Down, Right, Right)), (Block(Pos(0,2),Pos(0,2)),List(Right, Up, Left, Up, Right, Down)), (Block(Pos(2,2),Pos(2,2)),List(Right, Down, Left, Up, Right, Down)), (Block(Pos(3,3),Pos(3,4)),List(Down, Down, Right, Up, Right, Down)), (Block(Pos(2,2),Pos(2,2)),List(Left, Down, Right, Up, Right, Down)), (Block(Pos(2,5),Pos(2,5)),List(Right, Down, Right, Up, Right, Down)), (Block(Pos(0,1),Pos(0,2)),List(Up, Left, Up, Right, Right, Down)), (Block(Pos(2,1),Pos(2,2)),List(Down, Left, Up, Right, Right, Down)), (Block(Pos(1,0),Pos(1,0)),List(Left, Left, Up, Right, Right, Down)), (Block(Pos(2,4),Pos(2,5)),List(Down, Right, Up, Right, Right, Down)), (Block(Pos(0,1),Pos(0,1)),List(Up, Up, Left, Down, Down, Right)), (Block(Pos(1,0),Pos(2,0)),List(Left, Up, Left, Down, Down, Right)), (Block(Pos(1,2),Pos(2,2)),List(Right, Up, Left, Down, Down, Right)), (Block(Pos(1,3),Pos(2,3)),List(Left, Up, Right, Down, Down, Right)), (Block(Pos(1,5),Pos(2,5)),List(Right, Up, Right, Down, Down, Right)), (Block(Pos(4,5),Pos(4,6)),List(Down, Right, Right, Down, Down, Right)), (Block(Pos(3,7),Pos(3,7)),List(Right, Right, Right, Down, Down, Right)), (Block(Pos(2,0),Pos(2,0)),List(Down, Left, Up, Left, Down, Right)), (Block(Pos(2,2),Pos(2,2)),List(Down, Right, Up, Left, Down, Right)), (Block(Pos(4,5),Pos(4,6)),List(Down, Down, Right, Right, Down, Right)), (Block(Pos(3,7),Pos(3,7)),List(Right, Down, Right, Right, Down, Right)), (Block(Pos(3,7),Pos(4,7)),List(Down, Right, Right, Right, Down, Right)), (Block(Pos(4,6),Pos(4,7)),List(Right, Down, Right, Down, Right, Right)), (Block(Pos(4,6),Pos(4,6)),List(Down, Right, Right, Down, Right, Right)), (Block(Pos(2,7),Pos(3,7)),List(Right, Right, Right, Down, Right, Right)), (Block(Pos(3,2),Pos(3,2)),List(Left, Down, Down, Right, Up, Right, Down)), (Block(Pos(3,5),Pos(3,5)),List(Right, Down, Down, Right, Up, Right, Down)), (Block(Pos(3,5),Pos(4,5)),List(Down, Right, Down, Right, Up, Right, Down)), (Block(Pos(2,6),Pos(2,7)),List(Right, Right, Down, Right, Up, Right, Down)), (Block(Pos(0,0),Pos(0,0)),List(Left, Up, Left, Up, Right, Right, Down)), (Block(Pos(3,1),Pos(3,2)),List(Down, Down, Left, Up, Right, Right, Down)), (Block(Pos(2,3),Pos(2,3)),List(Right, Down, Left, Up, Right, Right, Down)), (Block(Pos(3,4),Pos(3,5)),List(Down, Down, Right, Up, Right, Right, Down)), (Block(Pos(2,3),Pos(2,3)),List(Left, Down, Right, Up, Right, Right, Down)), (Block(Pos(2,6),Pos(2,6)),List(Right, Down, Right, Up, Right, Right, Down)), (Block(Pos(0,0),Pos(0,0)),List(Up, Left, Up, Left, Down, Down, Right)), (Block(Pos(3,2),Pos(3,2)),List(Down, Right, Up, Left, Down, Down, Right)), (Block(Pos(3,3),Pos(3,3)),List(Down, Left, Up, Right, Down, Down, Right)), (Block(Pos(3,5),Pos(3,5)),List(Down, Right, Up, Right, Down, Down, Right)), (Block(Pos(4,7),Pos(4,7)),List(Right, Down, Right, Right, Down, Down, Right)), (Block(Pos(4,7),Pos(5,7)),List(Down, Right, Right, Right, Down, Down, Right)), (Block(Pos(3,8),Pos(3,9)),List(Right, Right, Right, Right, Down, Down, Right)), (Block(Pos(4,7),Pos(4,7)),List(Right, Down, Down, Right, Right, Down, Right)), (Block(Pos(4,7),Pos(5,7)),List(Down, Right, Down, Right, Right, Down, Right)), (Block(Pos(3,8),Pos(3,9)),List(Right, Right, Down, Right, Right, Down, Right)), (Block(Pos(5,7),Pos(5,7)),List(Down, Down, Right, Right, Right, Down, Right)), (Block(Pos(3,6),Pos(4,6)),List(Left, Down, Right, Right, Right, Down, Right)), (Block(Pos(3,8),Pos(4,8)),List(Right, Down, Right, Right, Right, Down, Right)), (Block(Pos(3,6),Pos(3,7)),List(Up, Right, Down, Right, Down, Right, Right)), (Block(Pos(5,6),Pos(5,7)),List(Down, Right, Down, Right, Down, Right, Right)), (Block(Pos(4,8),Pos(4,8)),List(Right, Right, Down, Right, Down, Right, Right)), (Block(Pos(4,7),Pos(4,8)),List(Right, Down, Right, Right, Down, Right, Right)), (Block(Pos(4,7),Pos(4,7)),List(Down, Right, Right, Right, Down, Right, Right)), (Block(Pos(2,8),Pos(3,8)),List(Right, Right, Right, Right, Down, Right, Right)), (Block(Pos(2,8),Pos(2,8)),List(Right, Right, Right, Down, Right, Up, Right, Down)), (Block(Pos(3,6),Pos(3,6)),List(Right, Down, Down, Right, Up, Right, Right, Down)), (Block(Pos(2,7),Pos(2,8)),List(Right, Right, Down, Right, Up, Right, Right, Down)), (Block(Pos(4,8),Pos(4,9)),List(Right, Right, Down, Right, Right, Down, Down, Right)), (Block(Pos(4,6),Pos(5,6)),List(Left, Down, Right, Right, Right, Down, Down, Right)), (Block(Pos(4,8),Pos(5,8)),List(Right, Down, Right, Right, Right, Down, Down, Right)), (Block(Pos(4,8),Pos(4,9)),List(Down, Right, Right, Right, Right, Down, Down, Right)), (Block(Pos(4,8),Pos(4,9)),List(Right, Right, Down, Down, Right, Right, Down, Right)), (Block(Pos(4,6),Pos(5,6)),List(Left, Down, Right, Down, Right, Right, Down, Right)), (Block(Pos(4,8),Pos(5,8)),List(Right, Down, Right, Down, Right, Right, Down, Right)), (Block(Pos(4,8),Pos(4,9)),List(Down, Right, Right, Down, Right, Right, Down, Right)), (Block(Pos(5,6),Pos(5,6)),List(Down, Left, Down, Right, Right, Right, Down, Right)), (Block(Pos(2,8),Pos(2,8)),List(Up, Right, Down, Right, Right, Right, Down, Right)), (Block(Pos(5,8),Pos(5,8)),List(Down, Right, Down, Right, Right, Right, Down, Right)), (Block(Pos(3,9),Pos(4,9)),List(Right, Right, Down, Right, Right, Right, Down, Right)), (Block(Pos(3,8),Pos(3,8)),List(Right, Up, Right, Down, Right, Down, Right, Right)), (Block(Pos(5,8),Pos(5,8)),List(Right, Down, Right, Down, Right, Down, Right, Right)), (Block(Pos(3,7),Pos(3,8)),List(Up, Right, Down, Right, Right, Down, Right, Right)), (Block(Pos(5,7),Pos(5,8)),List(Down, Right, Down, Right, Right, Down, Right, Right)), (Block(Pos(4,9),Pos(4,9)),List(Right, Right, Down, Right, Right, Down, Right, Right)), (Block(Pos(4,8),Pos(4,9)),List(Right, Down, Right, Right, Right, Down, Right, Right)), (Block(Pos(3,9),Pos(3,9)),List(Right, Up, Right, Down, Right, Right, Down, Right, Right))).toSet)
    }
  }
}
