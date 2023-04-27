package u06lab.solutions

import org.junit.Assert.assertEquals
import org.junit.Test

class SolitaireTest:

  @Test def findMovesTest(): Unit =
    assertEquals(Vector((0, 0), (0, 2), (2, 0), (2, 2)), Solitaire.findMoves(3, 3, (1, 1)))
    assertEquals(Vector((0, 2), (1, 1), (1, 3), (2, 0), (2, 4), (3, 1), (3, 3), (4, 2)), Solitaire.findMoves(5, 5, (2, 2)))
