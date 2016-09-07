module MainSpec where

import Test.Hspec
import Language.Haskell.TH
import DI
import SimpleDefs
import NotSoEasyToTestCode

import Data.Maybe
import Data.Time
import Data.IORef 
import Data.String.Utils
import Common

spec = do
  describe "" $ do
    specify "mapDeps" $ do

      let l x = Dep x []

      mapDeps (const "2" ) (Dep "1" []) `shouldBe` (Dep "2" [])
      mapDeps (const 2) (Dep "1" []) `shouldBe` (Dep 2 [])


    specify "convertDepsToExp" $ do

      -- let ppsB = shouldBeF pprint
      -- let ppsB = shouldBeF show
      let ppsB = shouldBe

      convertDepsToExp (Dep "a" []) `ppsB` (VarE $ mkName "a")

      convertDepsToExp (Dep "a" [Dep "b" []]) `ppsB`
        (AppE (VarE $ mkName "a") (VarE $ mkName "b"))

      convertDepsToExp (Dep "a" [Dep "b" [], Dep "c" []]) `ppsB`
        (AppE (AppE (VarE $ mkName "a") (VarE $ mkName "b")) (VarE $ mkName "c"))


    specify "override fn" $ do

      override "a" "b" (Dep "a" []) `shouldBe` Dep "b" []
      override "a" "c" (Dep "a" []) `shouldBe` Dep "c" []
      override "b" "c" (Dep "a" []) `shouldBe` Dep "a" []

      override "x" "c" (Dep "b" [Dep "a" []]) `shouldBe` (Dep "b" [Dep "a" []])
      override "b" "c" (Dep "b" [Dep "a" []]) `shouldBe` (Dep "c" [Dep "a" []])
      override "a" "c" (Dep "b" [Dep "a" []]) `shouldBe` (Dep "b" [Dep "c" []])

      override "a" "c" (Dep "b" [Dep "a" [], Dep "a" []]) `shouldBe` (Dep "b" [Dep "c" [], Dep "c" []])


    specify "assemble" $ do
      
      $(assemble barD) `shouldBe` 2


    specify "mocking" $ do

      let
        fooDMock = Dep "fooMock" []
        fooMock = 33 
      $(assemble $ override "foo" "fooMock" barD) `shouldBe` 34


    specify "type variable support" $ do

      $(assemble $ idTestD) `shouldBe` 3

      let
        idDMock = Dep "idMock" []
        idMock = (+1) 
      $(assemble $ override "id" "idMock" idTestD) `shouldBe` 4


    specify "module support" $ do
      $(assemble $ testModuleD) `shouldBe` 12

    specify "qualified names" $ do
      $(assemble $ Dep "Prelude.id" []) 1 `shouldBe` 1
      $(assemble $ Dep "Prelude.*" []) 2 3 `shouldBe` 6


    specify "code that is more real-life" $ do
      
      let parseTime t = fromJust $ parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" t
      mockConsole <- newIORef []
      cTime <- newIORef $ parseTime "2016-01-01 14:00:00"
      let
        putStrLnMockD = Dep "putStrLnMock" []
        putStrLnMock a = modifyIORef mockConsole (a :)
        
        -- readMockConsole = readIORef mockConsole >>= fmap reverse 
        readMockConsole = do
          readIORef mockConsole >>= (reverse .> return) 
          -- readIORef mockConsole $> (fmap reverse) 

        getCurrentTimeMockD = Dep "getCurrentTimeMock" []
        getCurrentTimeMock = readIORef cTime

        setUpThen cont = do
          writeIORef mockConsole [] 
          writeIORef cTime $ parseTime "2016-01-01 14:00:00"
          timer <- $(makeTimerD 
              $> override "putStrLn" "putStrLnMock"
              $> override "getCurrentTime" "getCurrentTimeMock"
              $> assemble
            )
          timer
          cont timer

      readMockConsole >>= (`shouldBe` [])
      
      setUpThen $ \timer -> do
        readMockConsole >>= (`shouldBe` ["2016-01-01 14:00:00 UTC"])

      setUpThen $ \timer -> do
        timer
        readMockConsole >>= (`shouldBe` ["2016-01-01 14:00:00 UTC", "2016-01-01 14:00:00 UTC, diff: 0s"])

      setUpThen $ \timer -> do
        writeIORef cTime $ parseTime "2016-01-01 14:00:01"
        timer
        readMockConsole >>= (`shouldBe` [ "2016-01-01 14:00:00 UTC", "2016-01-01 14:00:01 UTC, diff: 1s"])

      -- [x] TODO figure out a way to branch out just like with Jasmine-Given JS testing framework
      setUpThen $ \timer -> do
        writeIORef cTime $ parseTime "2016-01-01 14:00:00.00002"
        timer
        readMockConsole >>= (`shouldBe` [ "2016-01-01 14:00:00 UTC", "2016-01-01 14:00:00.00002 UTC, diff: 0.00002s"])



    specify "automatic deps declaration" $ do
      -- deps "makeTimer" $> runQ >>= (pprint  .> (`shouldBe` "Dep \"makeTimer\" [putStrLnD, getCurrentTimeD]"))
      -- putStrLn $( (fmap show $ location) >>= ( StringL .> LitE .> return)  )
      -- let asd = fmap (LitE $ StringL) getContentOfNextLine
      strip $(getContentOfNextLineLit) `shouldBe` "let asd foo = foo + 1"
      let asd foo = foo + 1
      strip $(getContentOfNextLineLit) `shouldBe` "let asd foo = foo + 2"
      let asd foo = foo + 2
      let 
        a = strip $(getContentOfNextLineLit) `shouldBe` "asd foo = foo + 2"
        asd foo = foo + 2
      a
      -- parseLineToDeps "foo = 1" `shouldBe` Dep "foo" []

      parseLineToDeps "a = 1" `shouldBe` ("a", "aD", [])
      parseLineToDeps "b = 1" `shouldBe` ("b", "bD", [])
      parseLineToDeps "b a = 1" `shouldBe` ("b", "bD", ["aD"])

      (injectableI (return "asd = 2") $> runQ $> fmap pprint) >>= (`shouldBe` "asdD = Dep \"asd\" []") 
      (injectableI (return "asd a = 2") $> runQ $> fmap pprint) >>= (`shouldBe` "asdD = Dep \"asd\" [aD]") 
      
      (injLeaf "asdasd" $> runQ $> fmap pprint) >>= (`shouldBe` "asdasdD = Dep \"asdasd\" []") 

      return ()

