{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Map as Map
import Data.Dynamic
import Data.Maybe

($>) = flip ($)

-- data Injectable = forall a. MkInjectable a
-- data Injectable = forall a. Show a => MkInjectable (Int -> a)
-- data AnyInjectable = forall a. Show a => MkInjectable ((String -> a) -> a)

-- instance Show Injectable where
--   show = 


-- instance Show Showable
--   where
--   showsPrec p (MkShowable a) = showsPrec p a


config :: Map String (Int -> Dynamic)
config = Map.fromList [
    ("b", \_ -> toDyn (2 :: Int)),
    ("c", \_ -> toDyn "asd"),
    ("d", \_ -> toDyn $ (assemble "b" $> fromDynamic $> fromJust :: Int) + 2 )
  ]

-- instance Injectable AnyInjectable where
--   pack = MkInjectable
--   unpack (MkInjectable a) = a 

-- class Injectable a where
--   pack :: a -> Injectable
--   unpack :: Injectable -> a

-- assemble :: (Assemble a)
-- assemble :: forall a. String -> a
assemble key = (method) 1
  where
  method = config Map.! key 

main :: IO ()
main = do
  -- print $ (assemble "b" $> fromDynamic :: Maybe Int)
  print $ (assemble "d" $> fromDynamic :: Maybe Int)
 
