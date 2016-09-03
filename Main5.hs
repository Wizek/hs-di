{-# LANGUAGE ExistentialQuantification #-}

data Showable = forall a . Show a => MkShowable a


instance Show Showable
  where
  showsPrec p (MkShowable a) = showsPrec p a

 
--
-- And a nice existential builder
--
pack :: Show a => a -> Showable
pack = MkShowable
 
--
-- A heteoregenous list of Showable values
--
hlist :: [Showable]
hlist = [ pack 3
        , pack 'x'
        , pack pi
        , pack "string"
        , pack (Just ()) ]
 
--
-- The only thing we can do to Showable values is show them
--
main :: IO ()
main = print hlist
    where
        f (MkShowable a) = show a
 
