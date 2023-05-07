package u06lab.solutions

import java.util.OptionalInt

/**
 * Board:
 * y
 *
 * 3
 * 2
 * 1
 * 0
 * 0 1 2 3 <-- x
 */
object ConnectThree extends App:
  val bound = 3

  type Game = Seq[Board]
  type Board = Seq[Disk]
  case class Disk(x: Int, y: Int, player: Player)
  enum Player:
    case X, O
    def other: Player = this match
      case X => O
      case _ => X

  extension(disks: Seq[Disk])
    private def countNears: Int =
      val sortedDisks = disks.sortBy(d => (d.x, d.y))
      sortedDisks.zip(sortedDisks.drop(1)).count((d1, d2) => (d1.x - d2.x).abs <= 1 && (d1.y - d2.y).abs <= 1) + 1

  extension(board: Board)
    private def inWith(criteria: Disk => Boolean): Seq[Disk] = board.filter(criteria(_))

    private def inLineWith(disk: Disk): Seq[Disk] = inWith(d => d.y == disk.y)

    private def inColumnWith(disk: Disk): Seq[Disk] = inWith(d => d.x == disk.x)

    private def inDiagonalWith(disk: Disk): Seq[Disk] = inWith(d => (d.x - disk.x).abs == (d.y - disk.y).abs)

    private def inMainDiagonalWith(disk: Disk): Seq[Disk] = inDiagonalWith(disk).filter(d => d.x - disk.x >= 0)

    private def inAntiDiagonalWith(disk: Disk): Seq[Disk] = inDiagonalWith(disk).filter(d => d.x - disk.x <= 0)

    def hasTris: Boolean = board.exists(d =>
      Seq(inColumnWith(d), inLineWith(d), inMainDiagonalWith(d), inAntiDiagonalWith(d))
        .map(_.filter(_.player == d.player))
        .exists(_.countNears >= 3)
    )

    def find(x: Int, y: Int): Option[Player] = board find (d => d.x == x && d.y == y) map (_.player)

    def firstAvailableRow(x: Int): Option[Int] = board.filter(_.x == x).maxByOption(_.y) match
      case None => Some(0)
      case Some(Disk(_, y, _)) if y == bound => None
      case s => s.map(_.y + 1)

    def placeAnyDisk(player: Player): Seq[Board] =
      for
        x <- bound to 0 by -1
        y = board.firstAvailableRow(x)
        if y.isDefined
      yield board :+ Disk(x, y.get, player)

  import Player.*


  def computeAnyGame(player: Player, numOfMoves: Int): LazyList[Game] = numOfMoves match
    case 1 => LazyList.from(Seq().placeAnyDisk(player)) map (Seq(_))
    case _ =>
      for
        game <- computeAnyGame(player.other, numOfMoves - 1)
        newBoard <- if game.last.hasTris then Seq(game.last) else game.last.placeAnyDisk(player)
      yield if game.last.hasTris then game else game :+ newBoard

  def printBoards(game: Game): Unit =
    for
      y <- bound to 0 by -1
      board <- game.reverse
      x <- 0 to bound
    do
      print(board.find(x, y).map(_.toString).getOrElse("."))
      if x == bound then
        print(" ")
        if board == game.head then println()

  computeAnyGame(O, 16).zipWithIndex.foreach { g =>
    println(f"Solution ${g._2}")
    printBoards(g._1)
    println()
  }
