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

import Data.Map as Map
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

--   override (Leaf "a") "a" "b" `shouldBe` Leaf "b"

-- override

shouldBe actual expected = print $ actual == expected
