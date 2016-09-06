{-# LANGUAGE TemplateHaskell #-}

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

idD = Dep "id" []

inj
idTest id = id 1 + 2  


testModuleD = Dep "testModule" [testImportD]
testModule testImport = testImport + 3
