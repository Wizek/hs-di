{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Map as Map

-- data Injectable = forall a. MkInjectable a
data Injectable = forall a. Show a => MkInjectable (Int -> a)
-- data AnyInjectable = forall a. Show a => MkInjectable ((String -> a) -> a)

-- instance Show Injectable where
--   show = 


-- instance Show Showable
--   where
--   showsPrec p (MkShowable a) = showsPrec p a


config :: Map String Injectable
config = Map.fromList [
    ("b", MkInjectable $ \_ -> 2),
    ("c", MkInjectable $ \_ -> "asd")
  ]

-- instance Injectable AnyInjectable where
--   pack = MkInjectable
--   unpack (MkInjectable a) = a 

-- class Injectable a where
--   pack :: a -> Injectable
--   unpack :: Injectable -> a

-- assemble :: (Assemble a)
-- assemble :: forall a. String -> a
assemble key = (case method of (MkInjectable a) -> a) 1
  where
  method = config Map.! key 

main :: IO ()
main = do
  print $ (assemble "b" :: Int)
 
