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
import qualified GradualSpec as G ()
import Control.Exception (evaluate)
import NeatInterpolation
import qualified Data.Text as T
import Control.DeepSeq (force)
import Language.Haskell.Meta
import Assert

inj
testIdiomaticImportMock = 44

inj
a :: Int
a = 1

injG
bI :: Int
bI = 2

spec = do
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
      (`shouldSatisfy` ("asdD = Dep \"asd\" []" `isPrefixOf`))
    (injectableI (return "asd a = 2") $> runQ $> fmap pprint) >>=
      (`shouldSatisfy` ("asdD = Dep \"asd\" [aD]" `isPrefixOf`))

    (injLeaf "asdasd" $> runQ $> fmap pprint) >>=
      (`shouldSatisfy` ("asdasdD = Dep \"asdasd\" []" `isPrefixOf`))

    return ()

  describe "idiomatic module support" $ do
    specify "utils" $ do
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
    specify "the real deal" $ do
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

    specify "less clumsy, requires more imports though" $ do
      let
        aD = Dep "a" []
        a = 2
      $( testIdiomaticModuleD
        $> override "testIdimoaticImport" "a"
        $> assembleSimple) `shouldBe` 6

    specify "make sure that inj also declares a value that does not require `assemble`" $ do
      testIdiomaticModuleA `shouldBe` 23

    describe "" $ do
      let f (n, _, _, _, ds)  = (n, ds)
      specify "Support for type declaration be in between inj and fn decl" $ do

        ([text|
          aI b = 1
        |] $> T.unpack $> parseLineToDepsG $> f) `shouldBe` ("a", ["b"])
        ("\naI b = 1" $> parseLineToDepsG $> f) `shouldBe` ("a", ["b"])
        ("\n\naI b = 1" $> parseLineToDepsG $> f) `shouldBe` ("a", ["b"])
        ([text|
          aI :: Int
          aI b = 1
        |] $> T.unpack $> parseLineToDepsG $> f) `shouldBe` ("a", ["b"])
        -- ("" $> parseLineToDepsG $> f) `shouldBe` ("", [])
        ("" $> parseLineToDepsG $> f $> force $> evaluate) `shouldThrow` anyException
        ([text|
          aI :: x => Int
          aI b = 1
        |] $> T.unpack $> parseLineToDepsG $> f) `shouldBe` ("a", ["b"])

        a `shouldBe` 1
        b `shouldBe` 2

      [aa| ("aI = \\n -> f $ g n" $> parseLineToDepsG $> f) `shouldBe` ("a", []) |]
      [aa| ("aI = \\n -> f $ g n" $> parseLineToDepsG $> f) `shouldBe` ("a", []) |]
      [aa| ("aI = \\x -> f $ g x" $> parseLineToDepsG $> f) `shouldBe` ("a", []) |]
      [aa| ("aI = 1" $> parseLineToDepsG $> f) `shouldBe` ("a", []) |]
      [aa| ("aI b = 1" $> parseLineToDepsG $> f) `shouldBe` ("a", ["b"]) |]
      [aa| ("aI b = \\ x -> 1" $> parseLineToDepsG $> f) `shouldBe` ("a", ["b"]) |]
      [aa| ("aI = 1 > 1" $> parseLineToDepsG $> f) `shouldBe` ("a", []) |]

      context "support @ notation" $ do
        [aa| ("aI f@longName = 1" $> parseLineToDepsG $> f) `shouldBe` ("a", ["longName"]) |]
        -- [aa| ("aI f @ longName = 1" $> parseLineToDepsG $> f) `shouldBe` ("a", ["longName"]) |]
        -- [aa| ("aI (f@longName) = 1" $> parseLineToDepsG $> f) `shouldBe` ("a", ["longName"]) |]

      context "multiline support" $ do
        -- [aa| ([text|
        --   aI
        --     b
        --     = 1
        -- |\] $> T.unpack $> parseLineToDepsG $> f) `shouldBe` ("a", ["b"]) |]
        it "" $ do
          ([text|
            aI
              b
              = 1
          |] $> T.unpack $> parseLineToDepsG $> f) `shouldBe` ("a", ["b"])

        it "" $ do
          ([text|
            a :: Int -> Int
            aI
              b
              = 1
          |] $> T.unpack $> parseLineToDepsG $> f) `shouldBe` ("a", ["b"])

        it "" $ do
          ([text|
            a
              :: Int
              -> Int
            aI
              b
              = 1
          |] $> T.unpack $> parseLineToDepsG $> f) `shouldBe` ("a", ["b"])

        it "" $ do
          ([text|
            a
              :: Int
              -> Int
            aI
              b@longAssName
              = 1
          |] $> T.unpack $> parseLineToDepsG $> f) `shouldBe` ("a", ["longAssName"])


    -- TODO: Handle splices in aa
    -- [aa| $(assemble $ override "foo" "33" barD) `shouldBe` 34 |]
    specify "override with simple expressions" $ do
      $(assemble $ override "foo" "33" barD) `shouldBe` 34
      $(assemble $ override "foo" "1 + 2" barD) `shouldBe` 4

    specify "transpose [Dec] to `(TupP [Pat], TupP [Exp])`" $ do
      ("a = 1; b = 2" $> pd $> transposeDecsToPE $> fst)
        `shouldBe` ("(a, b)" $> pp)

      ("a = 1; b = 2" $> pd $> transposeDecsToPE $> snd)
        `shouldBe` ("(1, 2)" $> pe)

    -- -- [ ] TODO: Overcome GHC stage restriction:
    -- --   home/wizek/sandbox/exp-xml/exp-xml/hs-di/test/MainSpec.hs:289:18:
    -- --       GHC stage restriction:
    -- --         ‘aD’ is used in a top-level splice or annotation,
    -- --         and must be imported, not defined locally
    -- --       In the splice: $(assemble aD)

    -- specify "allow defining injectable value at non-top-level" $ do
    --   let
    --     $(injP) = $(injE)
    --     aI = 453

    --   $(assemble aD) `shouldBe` 453


    -- specify "override a non-leaf" $ do
    --   -- pending

    -- specify "override, change deps" $ do
    --   pending

-- runOnlyPrefix = ["!"]
runOnlyPrefix = [""]
specify a = if (any (`isPrefixOf` a) runOnlyPrefix)
  then Hspec.specify a
  else (\_->return ())

pd = parseDecs .> fromRight
pp = parsePat .> fromRight
pe = parseExp .> fromRight
fromRight (Right a) = a
