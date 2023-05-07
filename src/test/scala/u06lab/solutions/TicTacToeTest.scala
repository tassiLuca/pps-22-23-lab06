package u06lab.solutions

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TicTacToeTest extends AnyFlatSpec with Matchers:

  import TicTacToe.*
  import TicTacToe.Player.*

  "`find` method" should "be able to find the correct disk inside the board" in {
    find(Seq())((0, 0)) shouldBe empty
    find(Seq(Disk((0, 0), X), Disk((0, 1), O)))((1, 1)) shouldBe empty
    find(Seq(Disk((0, 0), X), Disk((0, 1), O)))((0, 1)) shouldBe Some(Disk((0, 1), O))
  }

  "Given a board `placeAnyDisk`" should "return all possible boards..." in {
    Seq[Disk]().placeAnyDisk(O) shouldBe Seq(
      Seq(Disk((0, 0), O)),
      Seq(Disk((0, 1), O)),
      Seq(Disk((0, 2), O)),
      Seq(Disk((1, 0), O)),
      Seq(Disk((1, 1), O)),
      Seq(Disk((1, 2), O)),
      Seq(Disk((2, 0), O)),
      Seq(Disk((2, 1), O)),
      Seq(Disk((2, 2), O)),
    )
    val board = Seq(Disk((0, 0), X), Disk((0, 1), O), Disk((1, 0), O), Disk((2, 2), O), Disk((2, 1), O))
    board.placeAnyDisk(X) shouldBe Seq(
      board :+ Disk((0, 2), X), 
      board :+ Disk((1, 1), X), 
      board :+ Disk((1, 2), X), 
      board :+ Disk((2, 0), X)
    )
  }
