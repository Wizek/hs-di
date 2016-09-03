-- {-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

import Data.Map as Map
import Data.Dynamic
import Data.Maybe

($>) = flip ($)

c = 2
b = inject "c" c + 2

d = "2"
e = inject "d" d ++ "2"

f = (++ "bar")
g = inject "f" f "foo"

i = inject "id" id "foo"
i2 = inject "id" id ["a"]

-- type Inject a = String -> Inject a -> a

-- inject :: String -> (String -> a) -> a
-- inject :: Inject a
-- inject :: String -> a -> a
inject label value = case label `Map.lookup` overrides of
  Just a -> a $> fromDynamic $> fromJust `asTypeOf` value
  _      -> value

overrides = 
  Map.fromList
    [ ("c", toDyn (3 :: Int))
    , ("d", toDyn ("3"))

    , ("f", toDyn (++ "baz"))
    -- , ("id", toDyn (++ "baz"))
    ]

main :: IO ()
main = do
  -- print $ (assemble "b" $> fromDynamic :: Maybe Int)
  print $ (inject "c" c :: Int)
  print $ (inject "b" b :: Int)

  print $ (inject "d" d)
  print $ (inject "e" e)

  print $ (inject "f" f "a")
  print $ (inject "g" g)

  print $ (inject "i" i)
  print $ (inject "i2" i2)
 
