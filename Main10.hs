{-

In this experiment I am trying to emulate the manual assembly
of deeply nested and injected dependencies with the help of TH
and config ADT 

-}

-- {-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -ddump-to-file #-}

import qualified Data.Map as Map
import Data.Dynamic
import Data.Maybe
import Main10TH

($>) = flip ($)

-- test = ("a", "b")

-- b = ("b", "c")
-- foo = ("foo", "c")

-- curry3 = $(curryN 3)

test2 = $(assemble barD)

foo = 1

bar foo = foo + 1

main :: IO ()
main = do
  print $(assemble barD)

  -- let 
  --   fooDMock = 33
  -- print $(assemble barDMock)

  override (Leaf "a") "a" "b" `shouldBe` Leaf "b"
  override (Leaf "a") "a" "c" `shouldBe` Leaf "c"
  override (Leaf "a") "b" "c" `shouldBe` Leaf "a"

  override (Cons "b" [Leaf "a"]) "x" "c" `shouldBe` (Cons "b" [Leaf "a"])
  override (Cons "b" [Leaf "a"]) "b" "c" `shouldBe` (Cons "c" [Leaf "a"])
  override (Cons "b" [Leaf "a"]) "a" "c" `shouldBe` (Cons "b" [Leaf "c"])

  override (Cons "b" [Leaf "a", Leaf "a"]) "a" "c" `shouldBe` (Cons "b" [Leaf "c", Leaf "c"])

  $(assemble barD) `shouldBe` 2

  let
    fooDMock = Leaf "fooMock"
    fooMock = 33 
  $(assemble $ override barD "foo" "fooMock") `shouldBe` 34

shouldBe actual expected | actual == expected = putStrLn $ "OK " ++ show actual
                         | otherwise          = error $ "FAIL " ++ show actual ++ " /= " ++ show expected
