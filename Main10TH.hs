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


-- getContentOfNextLineD = Dep "getContentOfNextLine" [] 
getContentOfNextLine :: Q String
getContentOfNextLine = do
  loc <- location 
  -- runIO $ print loc
  line <- runIO $ do
    file <- readFile $ loc_filename loc
    let
      (start, _) = loc_start loc
      l = file $> lines $> drop (start) $> head
    return l

  return line

getContentOfNextLineLit = getContentOfNextLine $> fmap (StringL .> LitE)

parseLineToDeps :: String -> (String, String, [String])
parseLineToDeps line = (name, nameD, deps)
  where
  ws = words line
  name = head ws
  args = takeWhile (/= "=") $ tail ws
  nameD = d name
  deps = map d args
  d n = n ++ "D"

inj = injectable

-- injectableD = Dep "injectableI" [parseLineToDepsD]
injectable :: Q [Dec]
injectable = injectableI getContentOfNextLine
injectableI getContentOfNextLine = do
  line <- getContentOfNextLine
  let
    (name, nameD, deps) = parseLineToDeps line
    identD :: Q Pat
    identD = return $ VarP $ mkName nameD
    nameStr :: Q Exp
    nameStr = return $ (StringL .> LitE) name
    listLiteral :: Q Exp
    listLiteral = return $ ListE $ map (mkName .> VarE) deps
    consDep = return $ ConE $ mkName "Dep"
  
  [d|$identD = $consDep $nameStr $listLiteral|]

r x = x .> return