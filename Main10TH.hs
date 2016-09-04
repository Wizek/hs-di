{-# LANGUAGE TemplateHaskell #-}
  
module Main10TH where

import Control.Monad
import Language.Haskell.TH

curryN :: Int -> Q Exp
curryN n = do
  f  <- newName "f"
  xs <- replicateM n (newName "x")
  let args = map VarP (f:xs)
      ntup = TupE (map VarE xs)
  return $ LamE args (AppE (AppE (VarE $ mkName "f") (VarE $ mkName "asd")) (VarE $ mkName "foo"))

assemble t = do
  -- return $ AppE (VarE $ mkName $ fst t) (let (a,b) = snd t in AppE (VarE $ mkName a) (VarE $ mkName b))
  -- return $ AppE (VarE $ mkName $ fst t) $(runQ [|(VarE $ mkName $ snd t)|])
  -- return $ AppE (VarE $ mkName $ fst t) $([|(VarE $ mkName $ snd t)|])
  return $ convertDefsToExp t

convertDefsToExp (Leaf name) = VarE $ mkName name
-- convertDefsToExp (Cons name []) = AppE (VarE $ mkName a) (VarE $ mkName b)
convertDefsToExp (Cons name (x:xs)) = AppE (VarE $ mkName name) (convertDefsToExp x)

data Defs = Leaf String | Cons String [Defs]
  deriving (Show, Eq)

fooD = Leaf "foo"
barD = Cons "bar" [fooD]
-- barDMock = Cons "bar" [fooDMock]

override (Leaf n) a b     = Leaf $ overrideName n a b
override (Cons n xs) a b  = Cons (overrideName n a b) (map (\x-> override x a b) xs)

overrideName n a b | n == a      = b
                   | otherwise   = n

