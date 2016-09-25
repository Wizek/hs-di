{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}
{-# language PatternSynonyms #-}

module DependencyInjector (
  module DependencyInjector,
  -- module Assembler,
  ) where

-- import Assembler
import Control.Monad
import Language.Haskell.TH
import Common
import Data.List as L
import Language.Haskell.Meta (parseExp)
import Data.Either
import System.IO.Unsafe
import qualified Data.Set as Set

assemble :: Deps -> Q Exp
assemble = convertDepsViaTuple .> return

assembleSimple :: Deps -> Q Exp
assembleSimple = convertDepsToExp .> return

convertDepsToExp :: Deps -> Exp
convertDepsToExp = id
  .> mapDepNames (parseExp .> either errF id)
  -- .> mapChildren reverse
  .> convertDepsToExp'
  where
    errF = ("Error parsing: " ++) .> error


convertDepsToExp' :: DepsG Exp -> Exp

convertDepsToExp' d@(Dep{kind=Monadic, cs=[]}) =
  -- AppE (VarE $ mkName "unsafePerformIO") $ convertDepsToExp' d{kind=Pure}
  -- error "xxx"
  convertDepsToExp' $ depOP (VarE $ 'unsafePerformIO) [d{kind=Pure}]

convertDepsToExp' (getDep -> (_, name,   [])) = name

-- convertDepsToExp' d@(Dep{kind=Monadic}) =
--   AppE (VarE $ mkName "unsafePerformIO") $ convertDepsToExp' d{kind=Pure}


convertDepsToExp' d@(Dep{kind=Monadic, cs=(_:_)}) =
  error "Children not yet supported in Monadic deps"

convertDepsToExp' (getDep -> (_, name, x:xs)) =
  convertDepsToExp' (depOP (AppE name (convertDepsToExp' x)) xs)


data DepsG a = Dep {name :: a, src :: DepSrc, kind :: DepKind, cs :: [DepsG a]}
  deriving (Show, Eq)

data DepSrc = Original | Replaced
  deriving (Show, Eq)
-- [ ] TODO consider makeing `override` handle the tuple too making the
--          distinction between original and overridden dependencies obsolete

data DepKind = Pure | Monadic
  deriving (Show, Eq)

type Deps = DepsG String
-- [ ] TODO conder redefining as
--          type Deps = DepsG Exp
--          or
--          type Deps = DepsG ExpQ

instance Ord a => Ord (DepsG a) where
  compare (getDep -> (_, n1, _)) (getDep -> (_, n2, _)) =
    compare n1 n2

mapDepNames :: (a -> b) -> DepsG a -> DepsG b
mapDepNames f (getDep -> (c, n, xs)) = c (f n) (map (mapDepNames f) xs)

-- mapDeps :: (DepsG a -> DepsG b) -> DepsG a -> DepsG b
mapDeps f d | (cons, n, xs) <- getDep d = f $ cons n (map (mapDeps f) xs)

mapChildren f (getDep -> (c, n, xs)) = c n (f $ map (mapChildren f) xs)

override :: String -> String -> Deps -> Deps
override a b d = if d == res then error errMsg else res
  where
  res = mapDeps (overrideDep a b) d
  errMsg = a ++ " not found while trying to override: " ++ (take 200 $ show d)
  -- If you would like to be able to do what this error prevents,
  -- pelase contact the maintainer of this package.

overrideName a b n | n == a      = b
                   | otherwise   = n

overrideDep a b d@(getDep -> (c, n, ds)) =
  if n == a then d{name=b, src=Replaced} else d


getContentOfNextLine :: Q String
getContentOfNextLine = do
  loc <- location
  -- runIO $ print loc
  line <- runIO $ do
    file <- readFile $ loc_filename loc
    let
      (start, _) = loc_start loc
      l = file $> lines $> drop start $> head
    return l
  return line

getContentOfNextLines :: Q String
getContentOfNextLines = do
  loc <- location
  -- runIO $ print loc
  line <- runIO $ do
    file <- readFile $ loc_filename loc
    let
      (start, _) = loc_start loc
      l = file $> lines $> drop start $> take 10 $> unlines
    return l
  return line

getContentOfFile :: Q String
getContentOfFile = do
  loc <- location
  runIO $ readFile $ loc_filename loc

getContentOfFollowingFnLine :: Q String
getContentOfFollowingFnLine =
  getContentOfNextLines >>= findFirstFnDecLine .> return

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



inj :: Q [Dec]
inj = injI getContentOfFollowingFnLine
injI getContentOfFollowingFnLine = do
  getContentOfFollowingFnLine
  >>= parseLineToDeps .> return
  >>= injDecs

injectableI = injI

injLeaf = injectableLeaf

injectableLeaf :: String -> Q [Dec]
injectableLeaf name = injDecs (name, nameD name, [], [])

injDecs (name, nameD, depsD, deps) =
  [d|
    $identD = $consDep $nameStr $(nce "Original") $(nce "Pure") $listLiteral
    $(return $ VarP $ mkName $ nameT $ name) =
      $(return $ TupE $ map (VarE . mkName) (name : map (++ "T") deps))
    $(return $ VarP $ mkName $ name ++ "A") =
      $(return $ convertDepsToExp $ depOP name (map (mapDepNames (++ "A")) (map (flip depOP []) deps)))
    $(return $ VarP $ mkName $ (++ "I") $ name) =
      $(return $ VarE $ mkName $ name)
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
    -- ne = return . VarE . mkName
    nce = return . ConE . mkName

nameD = (++ "D")
nameT = (++ "T")

r x = x .> return

convertDepsViaTuple deps@(name -> n) = LetE
  [ValD (tuplePattern deps) (NormalB (VarE $ mkName $ n ++ "T")) []]
  (convertDepsToExp deps)

tuplePattern d@(getDep -> (_, n, ds)) = tuplePattern' Set.empty d n ds $> fst

tuplePattern' set d n [] = inSet set d $ \set -> (wrapNameFor d, set)
tuplePattern' set d n ds = inSet set d $ \set ->
  let
    (ds', set') = foldr f ([], set) ds
    -- f (getDep -> (_, n, ds)) (ls, set) = isSet set n $ \set -> ()
    f d@(getDep -> (_, n, ds)) (ls, set) = let (d', set') = tuplePattern' set d n ds in (d':ls, set')
      -- inSetOrInsert n set (WildP:ls, set) $ \set -> ( set)

  in (TupP $ (wrapNameFor d) : ds', set')

inSetOrInsert a set o1 o2 =
  if a `Set.member` set
  then o1
  else o2 $ a `Set.insert` set

inSet set d x =
  if d `Set.member` set
  then (WildP, set)
  -- else x
  else x $ d `Set.insert` set



wrapNameFor (Dep n Original _ _) = VarP $ mkName n
wrapNameFor (Dep _ Replaced _ _) = WildP

getDepName (getDep -> (_, n, _)) = n
getDepDs   (getDep -> (_, _, ds)) = ds

getDep (Dep n s p ds) = ((\n' ds' -> Dep n' s p ds'), n, ds)



-- functions for injG

injG :: Q [Dec]
injG = injIG getContentOfFollowingFnLine
injIG getContentOfFollowingFnLine = do
  getContentOfFollowingFnLine
  >>= parseLineToDepsG .> return
  >>= injDecsG 'Pure

injMG :: Q [Dec]
injMG = do
  getContentOfFollowingFnLine
  >>= parseLineToDepsG .> return
  >>= injDecsG 'Monadic

injAllG :: Q [Dec]
injAllG = do
  getContentOfFile
  >>= lines
  .> groupByIndentation
  .> filter (concat .> words .> uncons .> maybe False (fst .> ("I" `isSuffixOf`)))
  .> map (unlines .> parseLineToDepsG .> injDecsG 'Pure)
  .> sequence
  .> fmap concat

-- parseLineToDepsG :: String -> (String, String, [String], [String])
parseLineToDepsG ls = (name, nameI, nameD, deps, args)
  where
  line = findFirstFnDecLine ls
  ws = words line
  name = nameI $> removeIname
  nameI = head ws
  args = map (reverse .> takeWhile (/= '@') .> reverse) $ takeWhile (/= "=") $ tail ws
  nameD = d name
  deps = map d args
  d n = n ++ "D"

groupByIndentation = id
  .> groupBy (const $ (" " `isPrefixOf`))

joinIndentedLines = id
  .> groupByIndentation
  .> map (intercalate "")

findFirstFnDecLine ls = ls
  $> lines
  $> joinIndentedLines
  $> L.find (("=" `L.isInfixOf`) `andf` (("=>" `L.isInfixOf`) .> not))
  $> maybe (error $ "Couldn't find function definition:\n" ++ ls) id

orf :: (a -> Bool) -> (a -> Bool) -> a -> Bool
orf f g x = f x || g x

andf :: (a -> Bool) -> (a -> Bool) -> a -> Bool
andf f g x = f x && g x


removeIname n = n $> reverse .> f .> reverse
  where
  f ('I':(a@(_:_))) = a
  f _ = error $ "Name must end with `I` suffix. e.g. `fooI` or `barI`: " ++ n

depOP n ds = Dep n Original Pure ds

injDecsG n (name, nameI, nameD, depsD, deps) =
  [d|
    $identD = $consDep $nameStr Original $(return $ ConE $ n) $listLiteral :: Deps
    $(return $ VarP $ mkName $ nameT $ name) =
      $(return $ TupE $ map (mkName .> VarE) ((name ++ "I") : map (++ "T") deps))
    $(return $ VarP $ mkName $ name ++ "A") =
      -- $(assemble $ depOP nameI (map (flip depOP []) deps))
      $(return $ convertDepsToExp $ depOP nameI (map (flip depOP []) deps))
    $(return $ VarP $ mkName $ name) =
      $(if n == 'Pure
        then return $ VarE $ mkName $ name ++ "A"
        else return $ AppE (VarE 'unsafePerformIO) (VarE $ mkName $ name ++ "A")
        )
  |]
  where
    identD :: Q Pat
    identD = return $ VarP $ mkName nameD
    nameStr :: Q Exp
    nameStr = name $> StringL $> LitE $> return
    listLiteral :: Q Exp
    listLiteral = return $ ListE $ map (mkName .> VarE) depsD
    consDep :: Q Exp
    consDep = return $ ConE $ mkName "Dep"

injP :: Q Pat
injP = injG >>= transposeDecsToPE .> fst .> return

injE :: Q Exp
injE = injG >>= transposeDecsToPE .> snd .> return

transposeDecsToPE :: [Dec] -> (Pat, Exp)
transposeDecsToPE decs = (pats, exps)
  where
  pats = TupP $ map (\(ValD p e _) -> p) decs
  exps = TupE $ map (\(ValD p (NormalB e) _) -> e) decs
