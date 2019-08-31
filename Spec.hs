-- file Spec.hs
import Test.Hspec

import Sudoku

main :: IO ()
main = hspec $ do
  describe "Sudoku.solve" $ do
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
