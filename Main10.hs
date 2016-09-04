{-
  In this experiment I am trying to emulate the manual assembly
  of deeply nested and injected dependencies with the help of TH
  and config ADTs

  (+) Supports values to be injected
  (+) Supports functions to be injected
  (++) Supports overriding of arbitrary number and depth of dependencies
  (++) Compile time type checking (despites strings being used, those too are checked)
  (+) Supports type variables 
  (+) Theoretically also supports surgically only overriding some subsets of dependencies

  (+) emulates how a human would do DI by hand, and does the hard work automatically

  (-) Limited module support, requires re-exporting
    (?) Could it be possible to pass the default dependencies along in `Deps` somehow?
      That could get rid of this issue
  (?) How is performance impacted? Does GHC notice `f (g x) (g x)`? 
  
  [x] TODO: make multiple argumetns work
  [ ] TODO: Simplify Deps
  [ ] TODO: reorder arguments of override
  [ ] TODO: look for a way to have full module support (without having to explicitly re-export and risk name-clashes)
-}

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -ddump-to-file #-}

import qualified Data.Map as Map
import Data.Dynamic
import Data.Maybe
import Main10TH
import Main10App
import Language.Haskell.TH


($>) = flip ($)

main :: IO ()
main = do
  putStrLn ""
  putStrLn "# mapDeps"

  mapDeps (const "2" ) (Leaf "1") `shouldBe` (Leaf "2")
  mapDeps (const 2) (Leaf "1") `shouldBe` (Leaf 2)
  
  putStrLn ""
  putStrLn "# convertDepsToExp"

  -- let ppsB = shouldBeF pprint
  let ppsB = shouldBeF show

  convertDepsToExp (Leaf "a") `ppsB` (VarE $ mkName "a")

  convertDepsToExp (Dep "a" [Leaf "b"]) `ppsB`
    (AppE (VarE $ mkName "a") (VarE $ mkName "b"))

  convertDepsToExp (Dep "a" [Leaf "b", Leaf "c"]) `ppsB`
    (AppE (AppE (VarE $ mkName "a") (VarE $ mkName "b")) (VarE $ mkName "c"))

  putStrLn ""
  putStrLn "# override fn"

  override (Leaf "a") "a" "b" `shouldBe` Leaf "b"
  override (Leaf "a") "a" "c" `shouldBe` Leaf "c"
  override (Leaf "a") "b" "c" `shouldBe` Leaf "a"

  override (Dep "b" [Leaf "a"]) "x" "c" `shouldBe` (Dep "b" [Leaf "a"])
  override (Dep "b" [Leaf "a"]) "b" "c" `shouldBe` (Dep "c" [Leaf "a"])
  override (Dep "b" [Leaf "a"]) "a" "c" `shouldBe` (Dep "b" [Leaf "c"])

  override (Dep "b" [Leaf "a", Leaf "a"]) "a" "c" `shouldBe` (Dep "b" [Leaf "c", Leaf "c"])

  putStrLn ""
  putStrLn "# assemble"
  
  $(assemble barD) `shouldBe` 2

  putStrLn ""
  putStrLn "# mocking"

  let
    fooDMock = Leaf "fooMock"
    fooMock = 33 
  $(assemble $ override barD "foo" "fooMock") `shouldBe` 34

  putStrLn ""
  putStrLn "# type variable support"

  $(assemble $ idTestD) `shouldBe` 3

  let
    idDMock = Leaf "idMock"
    idMock = (+1) 
  $(assemble $ override idTestD "id" "idMock") `shouldBe` 4

  putStrLn ""
  putStrLn "# module support"
  
  $(assemble $ testModuleD) `shouldBe` 12


shouldBe actual expected | actual == expected = putStrLn $ "OK " ++ show actual
                         | otherwise          = error $ "FAIL " ++ show actual ++ " /= " ++ show expected

shouldBeF f actual expected | actual == expected = putStrLn $ "OK " ++ f actual
                            | otherwise          = error $ "FAIL " ++ f actual ++ " /= " ++ f expected
