{-# LANGUAGE TemplateHaskell #-}
{-# language ViewPatterns #-}
{-# language PatternSynonyms #-}

module Main10TH where

import Control.Monad
import Language.Haskell.TH

(.>) = flip (.)
infixl 9 .>
{-# INLINE (.>) #-}

($>) = flip ($)
infixl 0 $>
{-# INLINE ($>) #-}

assemble t = do
  t $> convertDepsToExp $> return

convertDepsToExp :: Deps -> Exp
convertDepsToExp = id
  .> mapDeps (mkName .> VarE)
  -- .> mapChildren reverse
  .> convertDepsToExp'

convertDepsToExp' :: DepsG Exp -> Exp
convertDepsToExp' (Dep name []) = name 
convertDepsToExp' (Dep name (x:xs)) = convertDepsToExp' (Dep (AppE name (convertDepsToExp' x)) xs)

data DepsG a = Dep a [DepsG a]
  deriving (Show, Eq)

type Deps = DepsG String


mapDeps :: (a -> b) -> DepsG a -> DepsG b 
mapDeps f (Dep n xs) = Dep (f n) (map (mapDeps f) xs)

-- mapChildren :: (a -> b) -> DepsG a -> DepsG a
mapChildren f (Dep n xs) = Dep n (f $ map (mapChildren f) xs)

override a b d = mapDeps (overrideName a b) d

overrideName a b n | n == a      = b
                   | otherwise   = n

-- deps :: String -> Q [Dec]
deps s = [d|f = 1|]


getContentOfNextLine = do
  -- (fmap show $ location) >>= ( StringL .> LitE .> return)
  loc <- location 
  runIO $ print loc
  line <- runIO $ do
    file <- readFile $ loc_filename loc
    let
      (start, _) = loc_start loc
      l = file $> lines $> drop (start) $> head
    return l

  return $ LitE $ StringL line
  -- >>= ( (\a->runIO (print a)) >> return ("a" $> (StringL .> LitE))  )