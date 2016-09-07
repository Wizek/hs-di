module SimpleDefs
  ( module SimpleDefs
  , module DefsToTestModuleSupport
  ) where

import DI
import DefsToTestModuleSupport
import DefsToTestIdiomaticModuleSupport

inj
foo = 1

inj
bar foo = foo + 1


injLeaf "id"
inj
idTest id = id 1 + 2  

inj
testModule testImport = testImport + 3

inj
testIdiomaticModule testIdimoaticImport = testIdimoaticImport + 4