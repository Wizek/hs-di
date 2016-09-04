module Main10App
	( module Main10App
	, module Main10TestImport
	) where

import Main10TH
import Main10TestImport

fooD = Leaf "foo"
foo = 1

barD = Cons "bar" [fooD]
bar foo = foo + 1

idD = Leaf "id"

idTestD = Cons "idTest" [idD]
idTest id = id 1 + 2  


testModuleD = Cons "testModule" [testImportD]
testModule testImport = testImport + 3