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

pattern Leaf x <- Dep x []
  where Leaf x =  Dep x []


mapDeps :: (a -> b) -> DepsG a -> DepsG b 
mapDeps f (Leaf n)   = Leaf $ f n
mapDeps f (Dep n xs) = Dep (f n) (map (mapDeps f) xs)

-- mapChildren :: (a -> b) -> DepsG a -> DepsG a
mapChildren f (Leaf n)   = Leaf n
mapChildren f (Dep n xs) = Dep n (f $ map (mapChildren f) xs)


override (Leaf n) a b    = Leaf $ overrideName n a b
override (Dep n xs) a b  = Dep (overrideName n a b) (map (\x-> override x a b) xs)

overrideName n a b | n == a      = b
                   | otherwise   = n

