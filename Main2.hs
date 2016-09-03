{-# language NoMonomorphismRestriction #-}
import Data.Map as Map
-- b = ("B", [], \_ -> 2)

-- b = \_ -> 2
-- a = \b -> b + 1

-- id :: a -> a
-- id a = a

-- print ((id :: Int -> Int) 3)
-- print (id 3)

data Foo a = Foo a
  deriving (Show, Eq)

config :: Map String ((Assemble a) -> (Foo a))
config = Map.fromList [
    -- ("a", \a -> Foo ((get $ a "c" :: Integer) + 1)),
    ("b", \_ -> Foo 2),
    ("c", \_ -> Foo "asd")
  ]

-- shouldBe (assemble ({B: ["B", [], () => 3]}, A), 4)

type Assemble a = String -> (Foo a)

assemble :: (Assemble a)
assemble input = method assemble
  where
  method = config Map.! input 

get :: Foo a -> a
get (Foo a) = a


shouldBe actual expected = print $ actual == expected

main = do
  shouldBe (assemble "b") (Foo 2)
  shouldBe (assemble "a") (Foo 3)

  print (id 3)
  print (3)
  print (id "abc")
  print ("abc")
  print (Foo 1)
  print (Foo "asdasd")
  -- shouldBe (assemble "c") "asd"