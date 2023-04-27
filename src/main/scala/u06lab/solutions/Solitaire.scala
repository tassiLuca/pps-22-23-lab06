package u06lab.solutions

object Solitaire extends App:
  type Position = (Int, Int)
  type Solution = Iterable[Position]

  extension (p: Position)
    def distanceTo(q: Position): Int = (p._1 - q._1).abs + (p._2 - q._2).abs
    def isInLineWith(q: Position): Boolean = (p._1 == q._1) || (p._2 == q._2)
    def isInDiagonalWith(q: Position): Boolean = (p._1 - q._1).abs == (p._2 - q._2).abs

  def placePlayer(width: Int, height: Int)(n: Int = width * height): Iterable[Solution] = n match
    case 1 => LazyList(List((height / 2, width / 2)))
    case _ =>
      for
        marked <- placePlayer(width, height)(n - 1)
        x <- 0 until height
        y <- 0 until width
        m = (x, y)
        if isFeasible(m, marked)
      yield
        marked.toSeq :+ m

  private def isFeasible(p1: Position, ps: Solution): Boolean = !ps.exists(_ == p1) && canMoveTowards(p1, ps.last)

  private def canMoveTowards(p1: Position, p2: Position): Boolean =
    ((p1 isInLineWith p2) && (p1 distanceTo p2) == 3) || ((p1 isInDiagonalWith p2) && (p1 distanceTo p2) == 2)
//    ((p1._2 == p2._2 || p1._1 == p2._1) && (p1._1 - p2._1).abs + (p1._2 - p2._2).abs == 3) ||
//      ((p2._1 - p1._1).abs == (p2._2 - p1._2).abs && (p1._1 - p2._1).abs == 2)

  def render(solution: Solution, width: Int, height: Int): String =
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
                    number = solution.toSeq.indexOf((y, x)) + 1
          yield if number > 0 then "%-3d ".format(number) else "X   "
      yield row.mkString
    rows.mkString("\n")

  val sols = placePlayer(width = 7, height = 5)(35)
  sols.zipWithIndex foreach (s => println(f"solution ${s._2} \n${render(s._1, width = 7, height = 5)}"))
