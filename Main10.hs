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
    (+) At least it supports qualified names:
        *Main> $(assemble $ Dep "Prelude.id" []) 1
        1
        *Main> $(assemble $ Dep "Prelude.*" []) 2 3
        6

  (?) How is performance impacted? Does GHC notice `f (g x) (g x)`? 
  
  [x] TODO: make multiple argumetns work
  [x] TODO: Simplify Deps
  [x] TODO: reorder arguments of override
  [x] TODO: try with some real-life code
  [ ] TODO: Write quasi quoter or TH splicer that writes the `Deps` definitions too
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
import Main10RealCode 
import Data.Time
import Data.IORef 

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

  override "a" "b" (Dep "a" []) `shouldBe` Dep "b" []
  override "a" "c" (Dep "a" []) `shouldBe` Dep "c" []
  override "b" "c" (Dep "a" []) `shouldBe` Dep "a" []

  override "x" "c" (Dep "b" [Dep "a" []]) `shouldBe` (Dep "b" [Dep "a" []])
  override "b" "c" (Dep "b" [Dep "a" []]) `shouldBe` (Dep "c" [Dep "a" []])
  override "a" "c" (Dep "b" [Dep "a" []]) `shouldBe` (Dep "b" [Dep "c" []])

  override "a" "c" (Dep "b" [Dep "a" [], Dep "a" []]) `shouldBe` (Dep "b" [Dep "c" [], Dep "c" []])


  section "assemble"
  
  $(assemble barD) `shouldBe` 2


  section "mocking"

  let
    fooDMock = Dep "fooMock" []
    fooMock = 33 
  $(assemble $ override "foo" "fooMock" barD) `shouldBe` 34


  section "type variable support"

  $(assemble $ idTestD) `shouldBe` 3

  let
    idDMock = Dep "idMock" []
    idMock = (+1) 
  $(assemble $ override "id" "idMock" idTestD) `shouldBe` 4


  section "module support"
  $(assemble $ testModuleD) `shouldBe` 12

  section "qualified names"
  $(assemble $ Dep "Prelude.id" []) 1 `shouldBe` 1
  $(assemble $ Dep "Prelude.*" []) 2 3 `shouldBe` 6


  section "code that is more real-life"
  mockConsole <- newIORef []
  let parseTime t = fromJust $ parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" t
  cTime <- newIORef $ parseTime "2016-01-01 14:00:00"
  let
    putStrLnMockD = Dep "putStrLnMock" []
    putStrLnMock a = do modifyIORef mockConsole (a :)
    
    -- readMockConsole = readIORef mockConsole >>= fmap reverse 
    readMockConsole = do
      c <- readIORef mockConsole
      return $ reverse c 

    getCurrentTimeMockD = Dep "getCurrentTimeMock" []
    getCurrentTimeMock = readIORef cTime

  timer <- $(makeTimerD 
      $> override "putStrLn" "putStrLnMock"
      $> override "getCurrentTime" "getCurrentTimeMock"
      $> assemble
    )
  
  readMockConsole >>= (`shouldBe` [])
  timer
  readMockConsole >>= (`shouldBe` ["2016-01-01 14:00:00 UTC"])
  timer
  readMockConsole >>= (`shouldBe` ["2016-01-01 14:00:00 UTC", "2016-01-01 14:00:00 UTC, diff: 0s"])

  writeIORef cTime $ parseTime "2016-01-01 14:00:01"
  timer
  readMockConsole >>= (`shouldBe`
    [ "2016-01-01 14:00:00 UTC"
    , "2016-01-01 14:00:00 UTC, diff: 0s"
    , "2016-01-01 14:00:01 UTC, diff: 1s"
    ])

  -- [ ] TODO figure out a way to branch out just like with Jasmine-Given JS testing framework
  writeIORef cTime $ parseTime "2016-01-01 14:00:01.00002"
  timer
  readMockConsole >>= (`shouldBe`
    [ "2016-01-01 14:00:00 UTC"
    , "2016-01-01 14:00:00 UTC, diff: 0s"
    , "2016-01-01 14:00:01 UTC, diff: 1s"
    , "2016-01-01 14:00:01.00002 UTC, diff: 0.00002s"
    ])

shouldBe = shouldBeF show

shouldBeF f actual expected | actual == expected = putStrLn $ "OK " ++ f actual
                            | otherwise          = error $ "FAIL " ++ f actual ++ " /= " ++ f expected

-- ($>) = flip ($)

section name = do
  putStrLn ""
  putStrLn $ "# " ++ name 