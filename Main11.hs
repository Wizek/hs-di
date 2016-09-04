-- {-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -ddump-to-file #-}

import Data.Map as Map
import Data.Dynamic
import Data.Maybe

-- data Deps = Leaf a

Deps [a]
-- test = ("a", "b")

-- b = ("b", "c")
foo = ("foo", "c")

-- curry3 = $(curryN 3)

test2 = $(asd ("a", "foo"))

main :: IO ()
main = do
  print 1 