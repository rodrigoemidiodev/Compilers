module Main where

import           Interpreter
import           Test.Hspec
import           Data.List (sort, nub)

-- alguns programas sequênciais para testes

-- a = 5+3; b = a-2;
example1 =
  CompoundStm
  (AssignStm "a"
    (OpExp (NumExp 5) Plus (NumExp 3))
  )
  (AssignStm "b"
    (OpExp (IdExp "a") Minus (NumExp 2))
  )

  
-- a = 5+3; a++; b = a*2;
example2 =
  CompoundStm
  (AssignStm "a" (OpExp (NumExp 5) Plus (NumExp 3)))
  (CompoundStm
   (IncrStm "a")
   (AssignStm "b" (OpExp (IdExp "a") Times (NumExp 2)))
  )

-- a = 2; a++; b = a+1; c = b+1;
example3 =
  CompoundStm
  (AssignStm "a" (NumExp 2))
  (CompoundStm
   (IncrStm "a")
   (CompoundStm
    (AssignStm "b" (OpExp (IdExp "a") Plus (NumExp 1)))
    (AssignStm "c" (OpExp (IdExp "b") Plus (NumExp 1)))))



main :: IO ()
main = hspec $ do
  describe "idsStm" $ do
    it "example1" $
      nub (sort $ idsStm example1) `shouldBe` ["a", "b"]
    it "example2" $
      nub (sort $ idsStm example2) `shouldBe` ["a", "b"]
    it "example3" $
      nub (sort $ idsStm example3) `shouldBe` ["a", "b", "c"]

  describe "interpStm" $ do
    it "example1" $
       sort (interpStm example1 []) `shouldBe` [("a",8), ("b", 6)]
    it "example2" $
       sort (interpStm example2 []) `shouldBe` [("a",9), ("b", 18)]
    it "example3" $
       sort (interpStm example3 []) `shouldBe` [("a", 3), ("b", 4), ("c", 5)]

