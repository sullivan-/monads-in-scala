package monads.lisst

object tictactoe {

  object Position {

    val minPositionIndex = 1
    val maxPositionIndex = 9

    lazy val allPositions: Seq[Position] =
    (minPositionIndex to maxPositionIndex) map { Position(_) }

  }

  case class Position(positionIndex: Int) {
    assert(
      positionIndex >= Position.minPositionIndex &&
      positionIndex <= Position.maxPositionIndex)
  }

  object Board {

    def apply(): Board = new Board(Seq())

  }

  class Board private (private val moves: Seq[Position]) {

    def nextConfigs: Lisst[Board] = {
      val nextBoardsSeq: Seq[Board] = {
        Position.allPositions
      } filterNot {
        moves contains _
      } map {
        moves :+ _
      } map {
        new Board(_)
      }
      Lisst(nextBoardsSeq: _*)
    }

    override def equals(a: Any) = a match {
      case that: Board => this.moves equals that.moves
      case _ => false
    }

    override def hashCode = moves.hashCode

  }

}
