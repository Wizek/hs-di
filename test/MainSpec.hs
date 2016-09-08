module MainSpec where

import Test.Hspec hiding (specify)
import qualified Test.Hspec as Hspec (specify)
import Data.List

import Language.Haskell.TH
import DI
import SimpleDefs
import NotSoEasyToTestCode

import Data.Maybe
import Data.Time
import Data.IORef
import Data.String.Utils
import Common
import Control.Exception (evaluate)

inj
testIdiomaticImportMock = 44

spec = do
  describe "" $ do
    specify "mapDepNames" $ do

      let l x = Dep x []

      mapDepNames (const "2" ) (Dep "1" []) `shouldBe` (Dep "2" [])
      mapDepNames (const 2) (Dep "1" []) `shouldBe` (Dep 2 [])


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

      override "a" "b" (Dep "a" []) `shouldBe` Rep "b" []
      override "a" "c" (Dep "a" []) `shouldBe` Rep "c" []
      -- override "b" "c" (Dep "a" []) `shouldBe` Dep "a" []
      evaluate (override "b" "c" (Dep "a" [])) `shouldThrow` anyException

      -- override "x" "c" (Dep "b" [Dep "a" []]) `shouldBe` (Dep "b" [Dep "a" []])
      evaluate (override "x" "c" (Dep "b" [Dep "a" []])) `shouldThrow` anyException
      override "b" "c" (Dep "b" [Dep "a" []]) `shouldBe` (Rep "c" [Dep "a" []])
      override "a" "c" (Dep "b" [Dep "a" []]) `shouldBe` (Dep "b" [Rep "c" []])

      override "a" "c" (Dep "b" [Dep "a" [], Dep "a" []]) `shouldBe`
        (Dep "b" [Rep "c" [], Rep "c" []])


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

    -- specify "qualified names" $ do
    --   $(assemble $ Dep "Prelude.id" []) 1 `shouldBe` 1
    --   $(assemble $ Dep "Prelude.*" []) 2 3 `shouldBe` 6


    specify "code that is more real-life" $ do

      let parseTime t =
            fromJust $ parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" t
      mockConsole <- newIORef []
      cTime <- newIORef $ parseTime "2016-01-01 14:00:00"
      let
        -- $(inj)
        putStrLnMockD = Dep "putStrLnMock" []
        putStrLnMockT = putStrLnMock
        putStrLnMock a = modifyIORef mockConsole (a :)

        -- readMockConsole = readIORef mockConsole >>= fmap reverse
        readMockConsole = do
          readIORef mockConsole >>= (reverse .> return)
          -- readIORef mockConsole $> (fmap reverse)

        -- $(inj)
        getCurrentTimeMockD = Dep "getCurrentTimeMock" []
        getCurrentTimeMockT = getCurrentTimeMock
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
        readMockConsole >>= (`shouldBe`
          ["2016-01-01 14:00:00 UTC", "2016-01-01 14:00:00 UTC, diff: 0s"])

      setUpThen $ \timer -> do
        writeIORef cTime $ parseTime "2016-01-01 14:00:01"
        timer
        readMockConsole >>= (`shouldBe`
          [ "2016-01-01 14:00:00 UTC", "2016-01-01 14:00:01 UTC, diff: 1s"])

      -- [x] TODO figure out a way to branch out just like with Jasmine-Given JS testing framework
      setUpThen $ \timer -> do
        writeIORef cTime $ parseTime "2016-01-01 14:00:00.00002"
        timer
        readMockConsole >>= (`shouldBe`
          [ "2016-01-01 14:00:00 UTC", "2016-01-01 14:00:00.00002 UTC, diff: 0.00002s"])



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

      parseLineToDeps "a = 1" `shouldBe` ("a", "aD", [], [])
      parseLineToDeps "b = 1" `shouldBe` ("b", "bD", [], [])
      parseLineToDeps "b a = 1" `shouldBe` ("b", "bD", ["aD"], ["a"])

      (injectableI (return "asd = 2") $> runQ $> fmap pprint) >>=
        (`shouldBe` "asdD = Dep \"asd\" []\nasdT = (asd)")
      (injectableI (return "asd a = 2") $> runQ $> fmap pprint) >>=
        (`shouldBe` "asdD = Dep \"asd\" [aD]\nasdT = (asd, a)")

      (injLeaf "asdasd" $> runQ $> fmap pprint) >>=
        (`shouldSatisfy` ("asdasdD = Dep \"asdasd\" []" `isPrefixOf`))

      return ()

  describe "idiona" $ do
    specify "idiomatic module support utils" $ do
      -- (convertDepsViaTuple (Dep "a" []) $> runQ $> fmap pprint) `shouldReturn` "let a = aT in a"
      (tuplePattern (Dep "a" []) $> pprint) `shouldBe` "a"
      (tuplePattern (Dep "a" [Dep "b" []]) $> pprint) `shouldBe` "(a, b)"
      (convertDepsViaTuple (Dep "a" []) $> pprint) `shouldBe` "let a = aT\n in a"
      (convertDepsViaTuple (Dep "a" [Dep "b" []]) $> pprint) `shouldBe`
        "let (a, b) = aT\n in a b"
      (convertDepsViaTuple (Dep "a" [Dep "b" [Dep "d" []], Dep "c" []]) $> pprint)
        `shouldBe` "let (a, (b, d), c) = aT\n in a (b d) c"
      (convertDepsViaTuple (Rep "a" []) $> pprint) `shouldBe` "let _ = aT\n in a"
      (convertDepsViaTuple (Dep "a" [Rep "b" []]) $> pprint) `shouldBe`
        "let (a, _) = aT\n in a b"

    -- [x] TODO: warn or error if override didn't match anything
    --           e.g. compare before with after to check for EQ
    specify "idiomatic module support" $ do
      $(assemble testIdiomaticModuleD) `shouldBe` 23
      $( testIdiomaticModuleD
        $> override "testIdimoaticImport" "testIdiomaticImportMock"
        $> assemble) `shouldBe` 48

    specify "Still support clumsy fallback" $ do
      let
        aD = Dep "a" []
        aT = a
        a = 1
      $( testIdiomaticModuleD
        $> override "testIdimoaticImport" "a"
        $> assemble) `shouldBe` 5

    specify "" $ do
      let
        aD = Dep "a" []
        a = 2
      $( testIdiomaticModuleD
        $> override "testIdimoaticImport" "a"
        $> assembleSimple) `shouldBe` 6

-- runOnlyPrefix = ["!"]
runOnlyPrefix = [""]
specify a = if (any (`isPrefixOf` a) runOnlyPrefix)
  then Hspec.specify a
  else (\_->return ())
