package u06lab.solutions

object TicTacToe extends App:

  val size = 3

  type Game = Seq[Board]
  type Board = Seq[Disk]
  case class Disk(position: Position, player: Player)
  type Position = (Int, Int)
  enum Player:
    case X, O
    def other: Player = this match
      case X => O
      case _ => X

  extension(board: Board)
    def find(position: Position): Option[Disk] = board.find(_.position == position)
    def placeAnyDisk(player: Player): Seq[Board] =
      for
        x <- 0 until size
        y <- 0 until size
        if find((x, y)).isEmpty
      yield board :+ Disk((x, y), player)

  def computeAnyGame(player: Player, moves: Int = size * size): LazyList[Game] = moves match
    case 1 => LazyList.from(Seq().placeAnyDisk(player) map (d => Seq(d)))
    case _ =>
      for
        game <- computeAnyGame(player.other, moves - 1)
        disk <- game.last.placeAnyDisk(player)
      yield game :+ disk
