{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}
{-# language PatternSynonyms #-}

module DependencyInjector where

import Control.Monad
import Language.Haskell.TH
import Common

assemble :: Deps -> Q Exp
assemble = convertDepsViaTuple .> return

assembleSimple :: Deps -> Q Exp
assembleSimple = convertDepsToExp .> return

convertDepsToExp :: Deps -> Exp
convertDepsToExp = id
  .> mapDepNames (mkName .> VarE)
  -- .> mapChildren reverse
  .> convertDepsToExp'

convertDepsToExp' :: DepsG Exp -> Exp
convertDepsToExp' (getDep -> (_, name,   [])) = name
convertDepsToExp' (getDep -> (_, name, x:xs)) =
  convertDepsToExp' (Dep (AppE name (convertDepsToExp' x)) xs)

data DepsG a = Dep a [DepsG a] | Rep a [DepsG a]
  deriving (Show, Eq)

type Deps = DepsG String

mapDepNames :: (a -> b) -> DepsG a -> DepsG b
mapDepNames f (Dep n xs) = Dep (f n) (map (mapDepNames f) xs)
mapDepNames f (Rep n xs) = Rep (f n) (map (mapDepNames f) xs)

-- mapDeps :: (DepsG a -> DepsG b) -> DepsG a -> DepsG b
mapDeps f d | (cons, n, xs) <- getDep d = f $ cons n (map (mapDeps f) xs)

mapChildren f (Dep n xs) = Dep n (f $ map (mapChildren f) xs)

override :: String -> String -> Deps -> Deps
override a b d = if d == res then error errMsg else res
  where
  res = mapDeps (overrideDep a b) d
  errMsg = a ++ " not found while trying to override: " ++ (take 200 $ show d)
  -- If you would like to be able to do what this error prevents,
  -- pelase contact the maintainer of this package.

overrideName a b n | n == a      = b
                   | otherwise   = n

overrideDep a b d | (c, n, ds) <- getDep d =
  if n == a then (Rep b ds) else d


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

parseLineToDeps :: String -> (String, String, [String], [String])
parseLineToDeps line = (name, nameD, deps, args)
  where
  ws = words line
  name = head ws
  args = takeWhile (/= "=") $ tail ws
  nameD = d name
  deps = map d args
  d n = n ++ "D"

inj = injectable

injectable :: Q [Dec]
injectable = injectableI getContentOfNextLine
injectableI getContentOfNextLine = do
  getContentOfNextLine
  >>= parseLineToDeps .> return
  >>= injDecs

injLeaf = injectableLeaf

injectableLeaf :: String -> Q [Dec]
injectableLeaf name = injDecs (name, nameD name, [], [])

injDecs (name, nameD, depsD, deps) =
  [d|
    $identD = $consDep $nameStr $listLiteral
    $(return $ VarP $ mkName $ nameT $ name) =
      $(return $ TupE $ map (VarE . mkName) (name : deps))
  |]
  where
    identD :: Q Pat
    identD = return $ VarP $ mkName nameD
    nameStr :: Q Exp
    nameStr = return $ (StringL .> LitE) name
    listLiteral :: Q Exp
    listLiteral = return $ ListE $ map (mkName .> VarE) depsD
    consDep :: Q Exp
    consDep = return $ ConE $ mkName "Dep"

nameD = (++ "D")
nameT = (++ "T")

r x = x .> return

convertDepsViaTuple deps | n <- getDepName deps = LetE
  [ValD (tuplePattern deps) (NormalB (VarE $ mkName $ n ++ "T")) []]
  (convertDepsToExp deps)

tuplePattern d@(getDep -> (_, n, ds)) = tuplePattern' d n ds

tuplePattern' d n [] = wrapNameFor d
tuplePattern' d n ds = TupP $ (wrapNameFor d) : map tuplePattern ds

wrapNameFor (Dep n _) = VarP $ mkName n
wrapNameFor (Rep n _) = WildP

getDepName (getDep -> (_, n, _)) = n
getDepDs   (getDep -> (_, _, ds)) = ds

getDep (Dep n ds) = (Dep, n, ds)
getDep (Rep n ds) = (Rep, n, ds)

