{-# LANGUAGE TemplateHaskell #-}
  
module Main10TH where

import Control.Monad
import Language.Haskell.TH

($>) = flip ($)
(.>) = flip (.)

assemble t = do
  -- return $ AppE (VarE $ mkName $ fst t) (let (a,b) = snd t in AppE (VarE $ mkName a) (VarE $ mkName b))
  -- return $ AppE (VarE $ mkName $ fst t) $(runQ [|(VarE $ mkName $ snd t)|])
  -- return $ AppE (VarE $ mkName $ fst t) $([|(VarE $ mkName $ snd t)|])
  t $> convertDefsToExp $> return

convertDefsToExp :: Defs -> Exp
convertDefsToExp = mapDefs (mkName .> VarE) .> convertDefsToExp

convertDefsToExp' (Leaf name) = name
-- convertDefsToExp' (Cons name []) = AppE (VarE $ mkName a) (VarE $ mkName b)
convertDefsToExp' (Cons name (x:xs)) = AppE name (convertDefsToExp' x)

data DefsG a = Leaf a | Cons a [DefsG a]
  deriving (Show, Eq)

type Defs = DefsG String
  -- deriving (Show, Eq)


mapDefs :: (a -> b) -> DefsG a -> DefsG b 
mapDefs f (Leaf n)    = Leaf $ f n
mapDefs f (Cons n xs) = Cons (f n) (map (mapDefs f) xs)


-- barDMock = Cons "bar" [fooDMock]

override (Leaf n) a b     = Leaf $ overrideName n a b
override (Cons n xs) a b  = Cons (overrideName n a b) (map (\x-> override x a b) xs)

overrideName n a b | n == a      = b
                   | otherwise   = n

