-- file Spec.hs
import Test.Hspec

import Sudoku

main :: IO ()
main = hspec $ do
  describe "Sudoku.solve" $ do
    it "can solve sudoku (naked pairs)" $ do
       let grid = "400000938032094100095300240370609004529001673604703090957008300003900400240030709"
       let state@(Sudoku sgrid xs _) = newSolver (convert grid)
       let nstate@(Sudoku nsgrid nxs loki) = solve state
       length nxs `shouldBe` 0

    it "can solve sudoku (naked triples)" $ do
       let grid = "070008029002000004854020000008374200000000000003261700000090612200000400130600070"
       let state@(Sudoku sgrid xs _) = newSolver (convert grid)
       let nstate@(Sudoku nsgrid nxs loki) = solve state
       length nxs `shouldBe` 0

    it "can solve sudoku (hidden pairs)" $ do
       let grid = "000000000904607000076804100309701080008000300050308702007502610000403208000000000"
       let state@(Sudoku sgrid xs _) = newSolver (convert grid)
       let nstate@(Sudoku nsgrid nxs loki) = solve state
       length nxs `shouldBe` 0

    it "can solve sudoku (hidden triples)" $ do
       let grid = "000000000231090000065003100008924000100050006000136700009300570000010843000000000"
       let state@(Sudoku sgrid xs _) = newSolver (convert grid)
       let nstate@(Sudoku nsgrid nxs loki) = solve state
       length nxs `shouldBe` 0

    it "can solve sudoku (x-wing)" $ do
       let grid = "700600008800030000090000310006740005005806900400092100087000020000060009600008001"
       let state@(Sudoku sgrid xs _) = newSolver (convert grid)
       let nstate@(Sudoku nsgrid nxs loki) = solve state
       length nxs `shouldBe` 0

    it "can solve sudoku (y-wing)" $ do
       let grid = "014600300050000007090840100000400800600050009007009000008016030300000010009008570"
       let state@(Sudoku sgrid xs _) = newSolver (convert grid)
       let nstate@(Sudoku nsgrid nxs loki) = solve state
       length nxs `shouldBe` 0

    it "can solve sudoku (xyz-wing)" $ do
       let grid = "100002000050090204000006700034001005500908007800400320009600000306010040000700009"
       let state@(Sudoku sgrid xs _) = newSolver (convert grid)
       let nstate@(Sudoku nsgrid nxs loki) = solve state
       length nxs `shouldBe` 0

    it "can not solve sudoku (pointing pairs, box/line-reduction)" $ do
       let grid = "000921003009000060000000500080403006007000800500700040003000000020000700800195000"
       let state@(Sudoku sgrid xs _) = newSolver (convert grid)
       let nstate@(Sudoku nsgrid nxs loki) = solve state
       length nxs `shouldBe` 107
