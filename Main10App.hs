module Main10App
  ( module Main10App
  , module Main10TestImport
  ) where

import Main10TH
import Main10TestImport

fooD = Dep "foo" []
foo = 1

barD = Dep "bar" [fooD]
bar foo = foo + 1

idD = Dep "id" []

idTestD = Dep "idTest" [idD]
idTest id = id 1 + 2  


testModuleD = Dep "testModule" [testImportD]
testModule testImport = testImport + 3
