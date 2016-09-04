-- {-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -ddump-to-file #-}

import Data.Map as Map
import Data.Dynamic
import Data.Maybe

($>) = flip ($)

-- c i = 2
-- b i = i "c" c + 2

-- data Inj = Inj (String -> a -> a)

d :: (String -> a -> a) -> String
d i = "2"
-- e i = i "d" d ++ "2"

-- f i = (++ "bar")
-- g i = i "f" f "foo"

-- i i = i "id" id "foo"
-- i2 i = i "id" id ["a"]

-- type Inject a = String -> Inject a -> a

-- inject :: String -> (String -> a) -> a
-- inject :: Inject a
-- inject :: String -> a -> a
inject :: Overrides -> String -> ((String -> b -> b) -> a) -> a
inject overrides label value = case label `Map.lookup` overrides of
  Just a -> a $> fromDynamic $> fromJust `asTypeOf` value (inject overrides)
  _      -> value (inject overrides)

type Overrides = Map String Dynamic

overrides :: Overrides
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
  -- print $ (inject overrides "c" c :: Int)
  -- print $ (inject overrides "b" b :: Int)

  -- print $ (inject overrides "d" d)
  -- print $ (inject overrides "e" e)

  -- print $ (inject overrides "f" f "a")
  -- print $ (inject overrides "g" g)

  -- print $ (inject overrides "i" i)
  -- print $ (inject overrides "i2" i2)
 
  print $ (inject overrides "d" d :: String)
