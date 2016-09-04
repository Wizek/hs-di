{-# LANGUAGE TemplateHaskell #-}
{-# language ViewPatterns #-}
{-# language PatternSynonyms #-}

module Main10TH where

import Control.Monad
import Language.Haskell.TH

($>) = flip ($)
(.>) = flip (.)

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

