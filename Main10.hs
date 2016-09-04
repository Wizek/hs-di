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
  [x] TODO: Simplify Deps
  [ ] TODO: reorder arguments of override
  [ ] TODO: look for a way to have full module support (without having to explicitly re-export and risk name-clashes)
-}

{-# LANGUAGE TemplateHaskell #-}
{-# language NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -ddump-to-file #-}

import qualified Data.Map as Map
import Data.Dynamic
import Data.Maybe
import Main10TH
import Main10App
import Language.Haskell.TH

main :: IO ()
main = do

  section "mapDeps"

  let l x = Dep x []

  mapDeps (const "2" ) (Dep "1" []) `shouldBe` (Dep "2" [])
  mapDeps (const 2) (Dep "1" []) `shouldBe` (Dep 2 [])


  section "convertDepsToExp"

  -- let ppsB = shouldBeF pprint
  let ppsB = shouldBeF show

  convertDepsToExp (Dep "a" []) `ppsB` (VarE $ mkName "a")

  convertDepsToExp (Dep "a" [Dep "b" []]) `ppsB`
    (AppE (VarE $ mkName "a") (VarE $ mkName "b"))

  convertDepsToExp (Dep "a" [Dep "b" [], Dep "c" []]) `ppsB`
    (AppE (AppE (VarE $ mkName "a") (VarE $ mkName "b")) (VarE $ mkName "c"))


  section "override fn"

  override (Dep "a" []) "a" "b" `shouldBe` Dep "b" []
  override (Dep "a" []) "a" "c" `shouldBe` Dep "c" []
  override (Dep "a" []) "b" "c" `shouldBe` Dep "a" []

  override (Dep "b" [Dep "a" []]) "x" "c" `shouldBe` (Dep "b" [Dep "a" []])
  override (Dep "b" [Dep "a" []]) "b" "c" `shouldBe` (Dep "c" [Dep "a" []])
  override (Dep "b" [Dep "a" []]) "a" "c" `shouldBe` (Dep "b" [Dep "c" []])

  override (Dep "b" [Dep "a" [], Dep "a" []]) "a" "c" `shouldBe` (Dep "b" [Dep "c" [], Dep "c" []])


  section "assemble"
  
  $(assemble barD) `shouldBe` 2


  section "mocking"

  let
    fooDMock = Dep "fooMock" []
    fooMock = 33 
  $(assemble $ override barD "foo" "fooMock") `shouldBe` 34


  section "type variable support"

  $(assemble $ idTestD) `shouldBe` 3

  let
    idDMock = Dep "idMock" []
    idMock = (+1) 
  $(assemble $ override idTestD "id" "idMock") `shouldBe` 4


  section "module support"
  
  $(assemble $ testModuleD) `shouldBe` 12


shouldBe = shouldBeF show

shouldBeF f actual expected | actual == expected = putStrLn $ "OK " ++ f actual
                            | otherwise          = error $ "FAIL " ++ f actual ++ " /= " ++ f expected

($>) = flip ($)

section name = do
  putStrLn ""
  putStrLn $ "# " ++ name 