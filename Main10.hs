{-
  In this experiment I am trying to emulate the manual assembly
  of deeply nested and injected dependencies with the help of TH
  and config ADTs
-}

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -ddump-to-file #-}

import qualified Data.Map as Map
import Data.Dynamic
import Data.Maybe
import Main10TH
import Main10App

($>) = flip ($)

main :: IO ()
main = do
  putStrLn ""
  putStrLn "# override fn"

  override (Leaf "a") "a" "b" `shouldBe` Leaf "b"
  override (Leaf "a") "a" "c" `shouldBe` Leaf "c"
  override (Leaf "a") "b" "c" `shouldBe` Leaf "a"

  override (Cons "b" [Leaf "a"]) "x" "c" `shouldBe` (Cons "b" [Leaf "a"])
  override (Cons "b" [Leaf "a"]) "b" "c" `shouldBe` (Cons "c" [Leaf "a"])
  override (Cons "b" [Leaf "a"]) "a" "c" `shouldBe` (Cons "b" [Leaf "c"])

  override (Cons "b" [Leaf "a", Leaf "a"]) "a" "c" `shouldBe` (Cons "b" [Leaf "c", Leaf "c"])

  putStrLn ""
  putStrLn "# assemble"
  
  $(assemble barD) `shouldBe` 2

  putStrLn ""
  putStrLn "# mocking"

  let
    fooDMock = Leaf "fooMock"
    fooMock = 33 
  $(assemble $ override barD "foo" "fooMock") `shouldBe` 34


  putStrLn ""
  putStrLn "# type variable support"
  $(assemble $ idTestD) `shouldBe` 3

  let
    idDMock = Leaf "idMock"
    idMock = (+1) 
  $(assemble $ override idTestD "id" "idMock") `shouldBe` 4

shouldBe actual expected | actual == expected = putStrLn $ "OK " ++ show actual
                         | otherwise          = error $ "FAIL " ++ show actual ++ " /= " ++ show expected
