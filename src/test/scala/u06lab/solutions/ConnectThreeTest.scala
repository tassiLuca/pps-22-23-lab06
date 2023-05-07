package u06lab.solutions

import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import u06lab.solutions.ConnectThree
import org.junit.Test

class ConnectThreeTest :

  import ConnectThree.*
  import ConnectThree.Player.*

  @Test
  def testFind(): Unit =
    assertEquals(Some(X), find(List(Disk(0, 0, X)), 0, 0))
    assertEquals(Some(O), find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 0, 1))
    assertEquals(None, find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 1, 1))

  @Test
  def testFirstAvailableRow(): Unit =
    assertEquals(Some(0), firstAvailableRow(List(), 0))
    assertEquals(Some(1), firstAvailableRow(List(Disk(0, 0, X)), 0))
    assertEquals(Some(2), firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X)), 0))
    assertEquals(Some(3), firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X)), 0))
    assertEquals(None, firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, X)), 0))

  @Test
  def testPlaceAnyDisk(): Unit =
    // .... .... .... ....
    // .... .... .... ....
    // .... .... .... ....
    // ...X ..X. .X.. X...
    assertEquals(
      Seq(Seq(Disk(3, 0, X)), Seq(Disk(2, 0, X)), Seq(Disk(1, 0, X)), Seq(Disk(0, 0, X))), 
      placeAnyDisk(Seq(), X)
    )
    // .... .... .... ....
    // .... .... .... ....
    // ...X .... .... ....
    // ...O ..XO .X.O X..O
    val board = Seq(Disk(3, 0, O))
    assertEquals(
      Seq(board :+ Disk(3, 1, X), board :+ Disk(2, 0, X), board :+ Disk(1, 0, X), board :+ Disk(0, 0, X)), 
      placeAnyDisk(board, X)
    )

  @Test
  def testTris(): Unit =
    //   -----------------
    // 3 | X | X |   |   |
    // 2 |   | O |   |   |
    // 1 | O | O | X |   |
    // 0 |   |   |   |   |
    //   -----------------
    //     0   1   2   3
    val board = Seq(Disk(0, 3, X), Disk(1, 3, X), Disk(1, 1, O), Disk(1, 2, O), Disk(2, 1, X), Disk(0, 1, O))
    assertFalse(board.hasTris)
    //   -----------------
    // 3 | X | X |   |   |
    // 2 |   | O |   |   |
    // 1 | 0 | O | X |   |
    // 0 |   | X |   |   |
    //   -----------------
    //     0   1   2   3
    assertFalse((board :+ Disk(1, 0, X)).hasTris)
    //   -----------------
    // 3 | X | X |   |   |
    // 2 |   | O |   | X |
    // 1 | 0 | O | X |   |
    // 0 |   | X |   |   |
    //   -----------------
    //     0   1   2   3
    assertTrue((board :+ Disk(1, 0, X) :+ Disk(3, 2, X)).hasTris)
    //   -----------------
    // 3 | X | X |   |   |
    // 2 |   | O |   |   |
    // 1 | 0 | O | X |   |
    // 0 |   | O |   |   |
    //   -----------------
    //     0   1   2   3
    assertTrue((board :+ Disk(1, 0, O)).hasTris)
    //   -----------------
    // 3 | X | X |   | X |
    // 2 |   | O |   |   |
    // 1 | O | O | X |   |
    // 0 |   |   |   |   |
    //   -----------------
    //     0   1   2   3
    assertFalse((board :+ Disk(3, 3, X)).hasTris)
    //   -----------------
    // 3 | X | X | X | X |
    // 2 |   | O |   |   |
    // 1 | 0 | O | X |   |
    // 0 |   |   |   |   |
    //   -----------------
    //     0   1   2   3
    assertTrue((board :+ Disk(3, 3, X) :+ Disk(2, 3, X)).hasTris)
    //   -----------------
    // 3 | X | X | O |   |
    // 2 |   | O |   |   |
    // 1 | 0 | O | X |   |
    // 0 |   |   |   |   |
    //   -----------------
    //     0   1   2   3
    assertTrue((board :+ Disk(2, 3, O)).hasTris)
    //   -----------------
    // 3 | X | X |   |   |
    // 2 |   | O |   |   |
    // 1 | O | O | X |   |
    // 0 |   |   |   |   |
    //   -----------------
    //     0   1   2   3
    assertTrue((board :+ Disk(0, 2, O) :+ Disk(2, 0, O)).hasTris)
