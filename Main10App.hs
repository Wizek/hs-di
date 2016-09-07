{-# language TemplateHaskell #-}

module Main10App
  ( module Main10App
  , module Main10TestImport
  ) where

import Main10TH
import Main10TestImport

inj
foo = 1

inj
bar foo = foo + 1


injLeaf "id"
inj
idTest id = id 1 + 2  


inj
testModule testImport = testImport + 3
