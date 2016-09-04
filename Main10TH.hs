{-# LANGUAGE TemplateHaskell #-}
  
module Main10TH where

import Control.Monad
import Language.Haskell.TH

($>) = flip ($)
(.>) = flip (.)

assemble t = do
  t $> convertDefsToExp $> return

convertDefsToExp :: Defs -> Exp
convertDefsToExp = id
  .> mapDefs (mkName .> VarE)
  -- .> mapChildren reverse
  .> convertDefsToExp'

convertDefsToExp' :: DefsG Exp -> Exp
convertDefsToExp' (Leaf name) = name
convertDefsToExp' (Cons name [x]) = AppE name (convertDefsToExp' x)
convertDefsToExp' (Cons name (x:xs)) = convertDefsToExp' (Cons (AppE name (convertDefsToExp' x)) xs)

data DefsG a = Leaf a | Cons a [DefsG a]
  deriving (Show, Eq)

type Defs = DefsG String


mapDefs :: (a -> b) -> DefsG a -> DefsG b 
mapDefs f (Leaf n)    = Leaf $ f n
mapDefs f (Cons n xs) = Cons (f n) (map (mapDefs f) xs)

-- mapChildren :: (a -> b) -> DefsG a -> DefsG a
mapChildren f (Leaf n)    = Leaf n
mapChildren f (Cons n xs) = Cons n (f $ map (mapChildren f) xs)


override (Leaf n) a b     = Leaf $ overrideName n a b
override (Cons n xs) a b  = Cons (overrideName n a b) (map (\x-> override x a b) xs)

overrideName n a b | n == a      = b
                   | otherwise   = n

