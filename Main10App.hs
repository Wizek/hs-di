module Main10App where

import Main10TH

fooD = Leaf "foo"
foo = 1

barD = Cons "bar" [fooD]
bar foo = foo + 1

idD = Leaf "id"

idTestD = Cons "idTest" [idD]
idTest id = id 1 + 2  