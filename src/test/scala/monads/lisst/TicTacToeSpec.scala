package monads.lisst

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import monads.lisst.tictactoe.Board

class TicTacToeSpec extends FlatSpec with ShouldMatchers {
  
  behavior of "tictactoe.Board.nextConfigs"
  it should "should produce board configurations as in the Haskell Tic Tac Toe example" in {

    def tick(boards: Lisst[Board]): Lisst[Board] = boards flatMap { _.nextConfigs }

    def thirdConfigsVersion1(board: Board): Lisst[Board] = tick(tick(tick(Lisst(board))))

    def thirdConfigsVersion2(board: Board): Lisst[Board] =
      Lisst(board) flatMap { _.nextConfigs } flatMap { _.nextConfigs } flatMap { _.nextConfigs }

    def thirdConfigsVersion3(board: Board): Lisst[Board] =
      for(
        board0 <- Lisst(board);
        board1 <- board0.nextConfigs;
        board2 <- board1.nextConfigs;
        board3 <- board2.nextConfigs)
      yield board3

    def thirdConfigsVersion4(board0: Board): Lisst[Board] =
      for(
        board1 <- board0.nextConfigs;
        board2 <- board1.nextConfigs;
        board3 <- board2.nextConfigs)
      yield board3

    { thirdConfigsVersion1(Board())
    } should equal {
      thirdConfigsVersion2(Board())
    }

    { thirdConfigsVersion1(Board())
    } should equal {
      thirdConfigsVersion3(Board())
    }

    { thirdConfigsVersion1(Board())
    } should equal {
      thirdConfigsVersion4(Board())
    }

    def permutation(n: Int, r: Int): Int = Range(n, n - r, -1).fold(1)(_ * _)

    thirdConfigsVersion1(Board()).size should equal { permutation(9, 3) }

    import LisstMonad.KleisliComposition
    val nc: (Board) => Lisst[Board] = _.nextConfigs
    val nnc = nc kleisliCompose nc
    val nnnc = nnc kleisliCompose nc
    val thirdConfigsVersion5 = nnnc

    { thirdConfigsVersion1(Board())
    } should equal {
      thirdConfigsVersion5(Board())
    }

  }

}
