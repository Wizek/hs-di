{-# language ExtendedDefaultRules #-}
module MainSpec where

import            Test.Hspec as Hspec hiding  (specify, it)
import qualified  Test.Hspec as Hspec         (specify, it)
import qualified  Test.Hspec.Core.Runner as HC
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
import Control.Exception
import NeatInterpolation
import qualified Data.Text as T
import Control.DeepSeq (force)
import Language.Haskell.Meta
import Assert
import Language.Haskell.Ghcid
import Control.Concurrent
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent.MVar
import Foreign.Store
-- import Data.IORef
import qualified Data.Text.IO as T
-- import qualified Test.HUnit as HUnit
import Test.Hspec.Expectations
import Text.InterpolatedString.Perl6
import Data.String.Interpolate.Util
import SpecCommon
import Data.Monoid
import Text.Regex.TDFA
import TestCommon
import Data.String (IsString)
import Data.Char as Char
import qualified Cases
import Test.Hspec.Core.Runner as Hspec

inj
testIdiomaticImportMock = 44

inj
a :: Int
a = 1

injG
bI :: Int
bI = 2


mainColor = HC.hspecWith
  HC.defaultConfig{HC.configColorMode=HC.ColorAlways}
  $ specWith setUpGhcidCached

main = hspec spec

spec = specWith setUpGhcidUncached

specWith setUpGhcid = do
  let
    dep n c = Dep n Original Pure c
    rep n c = Dep n Replaced Pure c

  specify "mapDepNames" $ do

    let l x = dep x []

    mapDepNames (const "2" ) (dep "1" []) `shouldBe` (dep "2" [])
    mapDepNames (const 2) (dep "1" []) `shouldBe` (dep 2 [])

  specify "convertDepsToExp" $ do

    -- let ppsB = shouldBeF pprint
    -- let ppsB = shouldBeF show
    let ppsB = shouldBe

    convertDepsToExp (dep "a" []) `ppsB` (VarE $ mkName "a")

    convertDepsToExp (dep "a" [dep "b" []]) `ppsB`
      (AppE (VarE $ mkName "a") (VarE $ mkName "b"))

    convertDepsToExp (dep "a" [dep "b" [], dep "c" []]) `ppsB`
      (AppE (AppE (VarE $ mkName "a") (VarE $ mkName "b")) (VarE $ mkName "c"))


  specify "override fn" $ do

    override "a" "b" (dep "a" []) `shouldBe` rep "b" []
    override "a" "c" (dep "a" []) `shouldBe` rep "c" []
    -- override "b" "c" (dep "a" []) `shouldBe` dep "a" []
    evaluate (override "b" "c" (dep "a" [])) `shouldThrow` anyException

    -- override "x" "c" (dep "b" [dep "a" []]) `shouldBe` (dep "b" [dep "a" []])
    evaluate (override "x" "c" (dep "b" [dep "a" []])) `shouldThrow` anyException
    override "b" "c" (dep "b" [dep "a" []]) `shouldBe` (rep "c" [dep "a" []])
    override "a" "c" (dep "b" [dep "a" []]) `shouldBe` (dep "b" [rep "c" []])

    override "a" "c" (dep "b" [dep "a" [], dep "a" []]) `shouldBe`
      (dep "b" [rep "c" [], rep "c" []])


  specify "assemble" $ do

    $(assemble barD) `shouldBe` 2


  specify "mocking" $ do

    let
      fooDMock = dep "fooMock" []
      fooMock = 33
    $(assemble $ override "foo" "fooMock" barD) `shouldBe` 34


  specify "type variable support" $ do

    $(assemble $ idTestD) `shouldBe` 3

    let
      idDMock = dep "idMock" []
      idMock = (+1)
    $(assemble $ override "id" "idMock" idTestD) `shouldBe` 4


  specify "module support" $ do
    $(assemble $ testModuleD) `shouldBe` 12

  -- specify "qualified names" $ do
  --   $(assemble $ dep "Prelude.id" []) 1 `shouldBe` 1
  --   $(assemble $ dep "Prelude.*" []) 2 3 `shouldBe` 6


  specify "code from real-life" $ do
    mockConsole <- newIORef []
    cTime <- newIORef $ parseTime' "2000-01-01 00:00:00"
    let
      putStrLnMock a = modifyIORef mockConsole (a :)
      getCurrentTimeMock = readIORef cTime
      readMockConsole = readIORef mockConsole >>= reverse .> return

    timer <- $(makeTimerD
      $> override "putStrLn" "putStrLnMock"
      $> override "getCurrentTime" "getCurrentTimeMock"
      $> assemble)

    readMockConsole `shouldReturn` []

    timer
    readMockConsole `shouldReturn` ["2000-01-01 00:00:00 UTC"]

    writeIORef cTime $ parseTime' "2000-01-01 00:00:00.0001"
    timer
    readMockConsole `shouldReturn`
      ["2000-01-01 00:00:00 UTC", "2000-01-01 00:00:00.0001 UTC, diff: 0.0001s"]


  describe "automatic deps declaration" $ do
    -- deps "makeTimer" $> runQ >>= (pprint  .> (`shouldBe` "dep \"makeTimer\" [putStrLnD, getCurrentTimeD]"))
    -- putStrLn $( (fmap show $ location) >>= ( StringL .> LitE .> return)  )
    -- let asd = fmap (LitE $ StringL) getContentOfNextLine
    specify "" $ do
      strip $(getContentOfNextLineLit) `shouldBe` "let asd foo = foo + 1"
      let asd foo = foo + 1
      strip $(getContentOfNextLineLit) `shouldBe` "let asd foo = foo + 2"
      let asd foo = foo + 2
      let
        a = strip $(getContentOfNextLineLit) `shouldBe` "asd foo = foo + 2"
        asd foo = foo + 2
      a
      -- parseLineToDeps "foo = 1" `shouldBe` dep "foo" []
      parseLineToDeps "a = 1" `shouldBe` ("a", "aD", [], [])
      parseLineToDeps "b = 1" `shouldBe` ("b", "bD", [], [])
      parseLineToDeps "b a = 1" `shouldBe` ("b", "bD", ["aD"], ["a"])

      -- parseLineToDepsG "bI a = 1" `shouldBe` ("b", "bD", ["aD"], ["a"])

    [aa| (injectableI (return "asd = 2") $> runQ $> fmap pprint) >>=
      (`shouldSatisfy` ("asdD = Dep \"asd\" Original Pure []" `isPrefixOf`)) |]

    [aa| (injectableI (return "asd a = 2") $> runQ $> fmap pprint) >>=
      (`shouldSatisfy` ("asdD = Dep \"asd\" Original Pure [aD]" `isPrefixOf`)) |]

    [aa| (injLeaf "asdasd" $> runQ $> fmap pprint) >>=
      (`shouldSatisfy` ("asdasdD = Dep \"asdasd\" Original Pure []" `isPrefixOf`)) |]

  describe "idiomatic module support" $ do
    specify "tuplePattern" $ do
      (tuplePattern (dep "a" []) $> pprint) `shouldBe` "a"
      (tuplePattern (dep "a" [dep "b" []]) $> pprint) `shouldBe` "(a, b)"
      (tuplePattern (dep "a" [dep "b" [], dep "c" []]) $> pprint) `shouldBe` "(a, b, c)"
      -- (tuplePattern (dep "a" [dep "b" [], dep "b" []]) $> pprint) `shouldBe` "(a, b, _)"
      -- (tuplePattern (dep "a" [dep "b" [], dep "b" []]) $> pprint) `shouldSatisfy`
      --   ((=~ "b") .> (== 1))
      (tuplePattern (dep "a" [dep "b" [], dep "b" []]) $> pprint $>
        (=~ "b")) `shouldBe` (1 :: Int)

    specify "utils" $ do
      -- (convertDepsViaTuple (dep "a" []) $> runQ $> fmap pprint) `shouldReturn` "let a = aT in a"
      (convertDepsViaTuple (dep "a" []) $> pprint) `shouldBe` "let a = aT\n in a"
      (convertDepsViaTuple (dep "a" [dep "b" []]) $> pprint) `shouldBe`
        "let (a, b) = aT\n in a b"
      (convertDepsViaTuple (dep "a" [dep "b" [dep "d" []], dep "c" []]) $> pprint)
        `shouldBe` "let (a, (b, d), c) = aT\n in a (b d) c"
      (convertDepsViaTuple (rep "a" []) $> pprint) `shouldBe` "let _ = aT\n in a"

    [aa| (convertDepsViaTuple (dep "a" [rep "b" []]) $> pprint) `shouldBe`
        "let (a, _) = aT\n in a b" |]

    -- [x] TODO: warn or error if override didn't match anything
    --           e.g. compare before with after to check for EQ
    specify "the real deal" $ do
      $(assemble testIdiomaticModuleD) `shouldBe` 23
      $( testIdiomaticModuleD
        $> override "testIdimoaticImport" "testIdiomaticImportMock"
        $> assemble) `shouldBe` 48

    specify "Still support clumsy fallback" $ do
      let
        aD = dep "a" []
        aT = a
        a = 1
      $( testIdiomaticModuleD
        $> override "testIdimoaticImport" "a"
        $> assemble) `shouldBe` 5

    specify "less clumsy, requires more imports though" $ do
      let
        aD = dep "a" []
        a = 2
      $( testIdiomaticModuleD
        $> override "testIdimoaticImport" "a"
        $> assembleSimple) `shouldBe` 6

    specify "make sure that inj also declares a value that does not require `assemble`" $ do
      testIdiomaticModuleA `shouldBe` 23

    describe "" $ do
      let f (n, _, _, _, ds, _) = (n, ds)
      let f2 (n, ni, _, _, ds, _) = (n, ni, ds)
      specify "!!!!!! Support for type declaration be in between inj and fn decl" $ do

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

        [ab| ("aI b = 1" $> parseLineToDepsG $> f2)`shouldBe` ("a", "aI", ["b"]) |]
        [ab| ("aMI b = 1" $> parseLineToDepsG $> f2)`shouldBe` ("a", "aMI", ["b"]) |]

        [ab| ("aI (Inj b) = 1" $> parseLineToDepsG $> f)`shouldBe` ("a", ["b"]) |]

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
          -- threadDelay 1000000
          ([text|
            a
              :: Int
              -> Int
            aI
              b@longAssName
              = 1
          |] $> T.unpack $> parseLineToDepsG $> f) `shouldBe` ("a", ["longAssName"])

    -- let loadModule' g modName = do
    --       result <- exec g $ ":load test/" ++ modName ++ ".hs"
    --       -- if result $> last $> ("Ok, modules loaded" `isPrefixOf`)
    --       if result $> any ("Ok, modules loaded" `isPrefixOf`)
    --         then map printForward result $> sequence_
    --         else error $ "\n" ++ unlines result


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

  specify "!! function pattern support" $ do
    let
      fr = either error id
    -- specify "!! simple" $ do
    -- [ab| 1 `shouldBe` 2 |]
    -- ([p| x |] $> runQ $> fmap asdasd) `shouldReturn` "x"

    -- let test a b = (a $> parsePat fr .> extractPatInfo .> fst) ==! b
    let p = parsePat .> fr .> extractPatInfo .> fst
    [ab| p "a"           ==! "a" |]
    [ab| p "a@b"         ==! "b" |]
    [ab| p "a@b@c"       ==! "c" |]
    [ab| p "(a)"         ==! "a" |]
    [ab| p "(a@b)"       ==! "b" |]
    [ab| p "(a@b@c)"     ==! "c" |]
    -- [ab| p "(a@b)@c"  ==! "c" |]

    -- specify "!! advanced" $ do
    [ab| p "a@(b@c)"     ==! "c" |]
    [ab| p "a@(Inj c)"   ==! "c" |]
    [ab| p "(Inj (a@c))" ==! "c" |]
    [ab| p "Inj c"       ==! "c" |]
    [ab| p "Inj a@c"     ==! "c" |]
    [ab| p "Inj a@b@c"   ==! "c" |]
    [ab| p "InjIO a"     ==! "a" |]

    -- TODO: Consider explicitly allowing or disallowing this pattern
    -- specify "!! constructor deconstruction (?)" $ do
    [ab| p "(InjIO (Just a))"   ==! "a" |] $> experimental
    [ab| p "(InjIO (Just a@b))" ==! "b" |] $> experimental


    let p = parsePat .> fr .> extractPatInfo
    [ab| p "a"            ==! ("a", Nothing) |]
    [ab| p "(Inj a)"      ==! ("a", Just Pure) |]
    [ab| p "(InjIO a)"    ==! ("a", Just Monadic) |]

    [ab| p "(Inj b@a)"    ==! ("a", Just Pure) |]
    [ab| p "b@(Inj a)"    ==! ("a", Just Pure) |]
    [ab| p "Just (Inj a)" ==! ("a", Just Pure) |] $> experimental
    [ab| p "Inj (Just a)" ==! ("a", Just Pure) |] $> experimental

  specify "!! showDepTree" $ do
    pending
    [ab| showDepTree "a"           ==! "a" |]

  xspecify "!! function pattern support" $ do
    let
      f (FunD n [(Clause cs _ _)]) =
        ( show n
        , map extractPatInfo
            cs)
      f (ValD (VarP n) _ _) = (show n, [])
      f err = error $ show err

    readFile "./test/IOScenario.hs"
      >>= id
      .> lines
      .> groupByIndentation
      .> filter (concat .> words .>
        (uncons .> maybe False (fst .> ("I" `isSuffixOf`))
        `andf` ((!! 1) .> (/= "::"))))
      .> map (id
        .> unlines
        .> break (== '=') .> fst
        .> (<> "= undefined")
        .> parseDecs
        .> fmap (map ((,,) <$> f <*> ppr <*> id)))
      .> map fromRight
      .> concat
      .> map (\(a, b, c) -> do
        print a
        print b
        print c
        putStrLn "\n")
      .> sequence_

  specify "unindent" $ do
    [ab| unindent (unlines [
        "a"
      ]) `shouldBe` (unlines [
        "a"
      ]) |]

    [ab| unindent (unlines [
        "  a"
      ]) `shouldBe` (unlines [
        "a"
      ]) |]

    [ab| unindent (unlines [
        "  a"
      , "a"
      ]) `shouldBe` (unlines [
        "  a"
      , "a"
      ]) |]

    [ab| unindent (unlines [
        "  a"
      , " a"
      ]) `shouldBe` (unlines [
        " a"
      , "a"
      ]) |]

  specify "parseInjArgSigs" $ do
    [ab| parseInjArgSigs "fooI =" `shouldBe` [] |]
    [ab| parseInjArgSigs "fooI a =" `shouldBe` [("a", Nothing)] |]
    [ab| parseInjArgSigs "fooI (Inj b) =" `shouldBe` [("b", Just Pure)] |]
    [ab| parseInjArgSigs "fooI (Inj b) a =" `shouldBe` [("b", Just Pure), ("a", Nothing)] |]
    [ab| evaluate (parseInjArgSigs "fooI a (Inj b) =") `shouldThrow` anyException |]
    -- [ab| (parseInjArgSigs "fooI a (Inj b) =") `shouldBe` [] |]


  -- TODO Consider supporting out of order annotations (\x1 x2 x3 -> f x1 x3 x2)
  specify "annotationOrderCheck" $ do
    [ab| isAnnotationOrderValid [Just Pure, Nothing] `shouldBe` True |]
    [ab| isAnnotationOrderValid [Just Pure, Just Pure, Nothing]
      `shouldBe` True |]

    [ab| isAnnotationOrderValid [Nothing, Just Pure] `shouldBe` False |]
    [ab| isAnnotationOrderValid [Just Pure, Nothing, Just Pure]
      `shouldBe` False |]
    [ab| isAnnotationOrderValid [Nothing, Just Pure, Just Pure]
      `shouldBe` False |]



  context "ghcid" $ beforeAll setUpGhcid $ do

    it "ghcid test 1" $ \g -> do
      exec g "1+2" `shouldReturn` ["3"]

    it "ghcid test 2" $ \g -> do
      T.writeFile "tmp/GhcidTest2.hs" [text|
        module GhcidTest2 where
        asd2val = 33
      |]
      T.writeFile "tmp/GhcidTest.hs" [text|
        module GhcidTest where

        import GhcidTest2
        import DI

        injG
        xxxI = 123

        xxx2 = asd2val + 1
      |]
      -- exec g "import GhcidTest"
      -- exec g ":load test/GhcidTest.hs" >>= print
      -- exec g ":m + GhcidTest"
      loadModule' g "GhcidTest"
      exec g "xxx" `shouldReturn` ["123"]
      exec g "xxx2" `shouldReturn` ["34"]

    specify "common dependency" $ \g -> do
      -- pendingWith [qx|
      --   s
      --   d
      -- |]

      T.writeFile "tmp/Scenarios/CommonDependency.hs" [text|
        import DI

        aI = 1
        bI a = a + 2
        cI a = a + 4
        dI b c = b + c

        injAllG
      |]
      loadModule' g "Scenarios/CommonDependency"
      execAssert g "$(assemble dD)" (`shouldBeStr` unlines ["8"])

      -- let
      --   (_, (_, aT), _) = dT
      --   aA = aT

    specify "common dependency 2" $ \g -> do
      T.writeFile "tmp/Scenarios/CommonDependency2.hs" [text|
        import DI

        xI = 1
        aI x = x + 1
        bI a = a + 2
        cI a = a + 4
        dI b c = b + c

        injAllG
      |]
      loadModule' g "Scenarios/CommonDependency2"

      execAssert g "$(assemble dD)" (`shouldBeStr` unlines ["10"])

      -- failDetails "foobar" $ 1 `shouldBe` 2

      -- 1 `shouldBe` 2
      --   $> failDetails "foobar"
      --   $> failDetails "asdasd"


    specify "injAllG" $ \g -> do
      T.writeFile "tmp/Scenarios/injAll.hs" [text|
        import DI

        aI = 1
        bI a = a + 2

        injAllG
      |]
      loadModule' g "Scenarios/injAll"
      execAssert g "$(assemble bD)" (`shouldBeStr` unlines ["3"])

    specify "injAllG out-of-order" $ \g -> do
      T.writeFile "tmp/Scenarios/injAll2.hs" [text|
        import DI

        bI a = a + 2
        aI = 1
        cI b = b + 3

        injAllG
      |]
      loadModule' g "Scenarios/injAll2"
      execAssert g "$(assemble cD)" (`shouldBeStr` unlines ["6"])

    -- http://stackoverflow.com/questions/7370073/testing-functions-in-haskell-that-do-io
    specify "SO" $ \g -> do
      T.writeFile "tmp/Scenarios/SO.txt" [qx|abc|]
      T.writeFile "tmp/Scenarios/SO.hs" [text|
        {-# language NoMonomorphismRestriction #-}
        module Scenarios.SO where
        import DI
        injLeaf "readFile"

        -- | 4) Counts the number of characters in a file
        inj
        numCharactersInFileA :: FilePath -> IO Int
        numCharactersInFile readFile = \fileName -> do
          contents <- readFile fileName
          return (length contents)
      |]
      T.writeFile "tmp/Scenarios/SOSpec.hs" [text|
        module Scenarios.SOSpec where
        import DI
        import Scenarios.SO

        main = do
          let readFileMock1 _ = return "x"
          $(assemble
            $ override "readFile" "readFileMock1"
            $ numCharactersInFileD) "tmp/Scenarios/SO.txt"
            >>= (`shouldBe` 1)

          let readFileMock2 a =  return ('x' : a)
          $(assemble
            $ override "readFile" "readFileMock2"
            $ numCharactersInFileD) "tmp/Scenarios/SO.txt"
            >>= (`shouldBe` 21)

          $(assemble numCharactersInFileD) "tmp/Scenarios/SO.txt"
            >>= (`shouldBe` 3)
            -- This last assertuion is here to demonstrate that the original
            -- behavior also works, but naturally we would want to do as
            -- little IO as possible in unit tests.

        -- assertion function
        shouldBe = shouldBeF show
        shouldBeF f actual expected | actual == expected = putStrLn $ "OK " ++ f actual
                                    | otherwise          = error $ "FAIL " ++ f actual ++ " /= " ++ f expected
      |]
      loadModule' g "Scenarios/SOSpec"
      execAssert g "main" (`shouldSatisfy` (lines .> last .> ("OK" `isPrefixOf`)))

    Hspec.runIO $ T.writeFile "tmp/Scenarios/SimpleShouldBe.hs" [text|
      module Scenarios.SimpleShouldBe where

      -- assertion function
      shouldBe = shouldBeF show
      shouldBeF f actual expected | actual == expected = putStrLn $ "OK " ++ f actual
                                  | otherwise          = error $ "FAIL " ++ f actual ++ " /= " ++ f expected
    |]

    describe ">>= support" $ do
      specify "! >>= at most once 1" $ \g -> do
        T.writeFile "tmp/Scenarios/BindOnce1.hs" [text|
          module Scenarios.BindOnce1 where
          import DI
          injAllG

          aI = return 1
          bI a = a
          cI a = a
          dI b c = do
            b <- b
            c <- c
            return $ b + c
          -- dI b c = (+) <$> b <*> c
        |]
        T.writeFile "tmp/Scenarios/BindOnce1Spec.hs" [text|
          import DI
          import ComposeLTR
          import Scenarios.SimpleShouldBe
          import Scenarios.BindOnce1
          import Data.IORef

          main = do
            $(assemble $ dD) >>= (`shouldBe` 2)

            counter <- newIORef 0
            $(dD
              $> override "a" "modifyIORef counter (+1) >> return 2"
              $> assemble) >>= (`shouldBe` 4)

            readIORef counter >>= (`shouldBe` 1)
        |]
        loadModule' g "Scenarios/BindOnce1Spec"
        execAssert g "main" (`shouldSatisfy` (lines .> last .> ("OK" `isPrefixOf`)))
          $> (const noop)

      specify "! >>= at most once 2" $ \g -> do
        pending
        T.writeFile "tmp/Scenarios/BindOnce2.hs" [text|
          module Scenarios.BindOnce2 where
          import DI

          -- injG
          -- aI = 1
          injMG
          aI = return 1

          injG
          bI a = a

          injG
          cI a = a

          injMG
          eI b = return $ 2 + b

          injG
          dI b c e = b + c

        |]
        T.writeFile "tmp/Scenarios/BindOnce2Spec.hs" [text|
          import DI
          import ComposeLTR
          import Scenarios.SimpleShouldBe
          import Scenarios.BindOnce2

          main = do
            $(assemble $ dD) >>= (`shouldBe` 2)

            -- $(assemble $ dD) >>= (`shouldBe` 2)

            -- counter <- newIORef 0
            -- $(dD
            --   $> override "a" "modifyIORef counter (+1) >> return 2"
            --   $> assemble) >>= (`shouldBe` 4)

            -- readIORef counter >>= (`shouldBe` 1)

        |]
        loadModule' g "Scenarios/BindOnce2Spec"
        execAssert g "main" (`shouldSatisfy` (lines .> last .> ("OK" `isPrefixOf`)))

      let
        -- ghcidTemplate :: (IsString a, IsString b) =>
        --   String -> String -> a -> b -> SpecWith Ghci
        -- ghcidTemplate :: _
        ghcidTemplate specDesc mainFileContent specFileContent = do
          specify (specDesc <> ", " <> modName) $ \g -> do
          -- specify [qc|$specDesc, $modName|] $ \g -> do
            -- \{-# OPTIONS_GHC -fno-defer-type-errors #-}
            T.writeFile [qc|tmp/Scenarios/{modName}.hs|] $ [qx|
              {"module"} Scenarios.{modName} where
              import DI

            |] <> mainFileContent
            --   {mainFileContent}
            -- |]
            T.writeFile [qc|tmp/Scenarios/{modName}Spec.hs|] $ [qx|
              import DI
              import ComposeLTR
              import Scenarios.SimpleShouldBe
              import Scenarios.{modName}

            |] <> specFileContent
            --   {specFileContent}
            -- |]
            loadModule' g [qc|Scenarios/{modName}Spec|]
            execAssert g "main" (`shouldSatisfy` (lines .> last .> ("OK" `isPrefixOf`)))
          where
          modName = specDesc $> T.pack $> Cases.camelize $> T.unpack
            $> (\(x:xs)-> Char.toUpper x : xs)

      specify "dependency for monadic value" $ \g -> do
        T.writeFile "tmp/Scenarios/BindOnce3.hs" [text|
          module Scenarios.BindOnce3 where
          import DI

          injG
          aI = 1

          injMG
          bI a = return $ a + 1

          injG
          cI b = b

        |]
        T.writeFile "tmp/Scenarios/BindOnce3Spec.hs" [text|
          import DI
          import ComposeLTR
          import Scenarios.SimpleShouldBe
          import Scenarios.BindOnce3

          main = do
            $(assemble $ cD) >>= (`shouldBe` 2)

        |]
        loadModule' g "Scenarios/BindOnce3Spec"
        execAssert g "main" (`shouldSatisfy` (lines .> last .> ("OK" `isPrefixOf`)))

      ghcidTemplate "!!!!! annotate deps" [qx|
          injAllG
          aI = 1
          bI (Inj a) = a + 1
        |] [qx|
          main = $(assemble bD) `shouldBe` 2
        |]

      ghcidTemplate "!!!!!!! annotate deps mixed" [qx|
          injAllG

          aI = 1
          b1I (Inj a) c = a + c
          b2I a = a
          -- b3I c (Inj a) = a + c
        |] [qx|
          main = do
            $(assemble b1D) 2 `shouldBe` 3
            $(assemble b2D) `shouldBe` 1
        |]

        -- cI (Unwrap b) = b
        -- cI (UnIO b) = b
        -- cI (InjIO b) = b
        -- cI (InjMaybeIO b) = b
        -- cI (InjMayIO b) = b
        -- cI (InjM b) = b
        -- cI (InjMayM b) = b

      ghcidTemplate "!!!!! dependee" [text|
          injG
          aI = return 1

          bD = Dep "b" Original Monadic [aD{kind=Monadic}, aD{kind=Pure}]
          bT = (bI, aT, aT)
          bI x y = y >>= \y' -> return $ x + 1 + y'
        |] [qx|
          main = $(assemble bD) >>= (`shouldBe` 3)
        |]

      ghcidTemplate "!!!!! dependee 2" [text|
          injG
          xI = 1

          injG
          aI x = return $ 1 + x

          bD = Dep "b" Original Monadic [aD{kind=Monadic}, aD{kind=Pure}]
          bT = (bI, aT, aT)
          bI x y = y >>= \y' -> return $ x + 1 + y'
        |] [qx|
          main = $(assemble bD) >>= (`shouldBe` 5)
        |]

      ghcidTemplate "!!!!!! dependee 3" [text|
          injAllG
          aI = return 1
          bI (InjIO a) = a + 1
        |] [qx|
          main = $(assemble bD) >>= (`shouldBe` 2)
        |]

          -- {-# OPTIONS_GHC -fno-deferred-type-errors #-}
      ghcidTemplate "!!!!!! dependee 4" [text|
          injAllG
          monadicI = return 1
          pureValI = 2
          fooI (Inj pureVal) = pureVal + 4
          foo2I (Inj foo) = foo + 8

          barI (InjIO monadic) = monadic + 16
          bar2I (Inj bar) = bar + 32

          bazI (Inj foo2) (Inj bar2) = foo2 + bar2
        |] [qx|
          main = do
            $(assemble bazD) >>= (`shouldBe` 63)
            -- bar2 `shouldBe` 47
        |]

      -- ghcidTemplate "!!!!!! dependee 5" [text|
      --     injAllG
      --     monadicI = return 1
      --     barI (InjIO monadic) = monadic + 16
      --     bar2I (Inj bar) = bar + 32
      --   |] [qx|
      --     main = do
      --       bar2 `shouldBe` 47
      --   |]

      specify "!!!! annotate deps" $ \g -> do
        T.writeFile "tmp/Scenarios/Bind4.hs" [text|
          module Scenarios.Bind4 where
          import DI
          injAllMG

          aMI = return 1
          bMI a = return $ a + 1
          cI b = b

        |]
        T.writeFile "tmp/Scenarios/Bind4Spec.hs" [text|
          import DI
          import ComposeLTR
          import Scenarios.SimpleShouldBe
          import Scenarios.Bind4

          main = do
            $(assemble $ cD) >>= (`shouldBe` 2)

        |]
        loadModule' g "Scenarios/Bind4Spec"
        execAssert g "main" (`shouldSatisfy` (lines .> last .> ("OK" `isPrefixOf`)))

      let modName = "Bind5"
      specify ("!!!! Unwrap root dependency, "<>modName) $ \g -> do
        T.writeFile [qc|tmp/Scenarios/{modName}.hs|] [qx|
          module Scenarios.{modName} where
          import DI
          injAllMG

          aMI = return 1
          bMI a = return $ a + 1

        |]
        T.writeFile [qc|tmp/Scenarios/{modName}Spec.hs|] [qx|
          import DI
          import ComposeLTR
          import Scenarios.SimpleShouldBe
          import Scenarios.{modName}

          main = do
            $(assemble $ bD) >>= (`shouldBe` 2)

        |]
        loadModule' g [qc|Scenarios/{modName}Spec|]
        execAssert g "main" (`shouldSatisfy` (lines .> last .> ("OK" `isPrefixOf`)))

    -- it ">>= support" $ \g -> do
    --   -- exec g ":load test/IOScenario.hs" >>= print
    --   loadModule' g "IOScenario"
    --   -- (exec g "$(assemble startupTimeStringD)" $> fmap unlines) >>= putStrLn
    --   -- (exec g "startupTimeStringD" $> fmap unlines) >>= putStrLn
    --   -- (exec g "$(assemble startupTimeStringD)" $> fmap unlines)
    --   --   `shouldReturn` unlines ["123"]

    --   (exec g "$(assemble xxxD)") >>= (unlines .> putStrLn)

    --   loadModule' g "IOScenarioMain"
    --   -- exec g "xxx" >>=  unlines .> putStrLn
    --   -- exec g "$(assemble xxxD)" >>=  unlines .> putStrLn

    --   -- exec g "1+1+12323" >>= map printForward .> sequence_
    --   -- print 1
    --   -- 1 `shouldBe` 2
    --   -- exec g "import Asd"
    --   -- exec g ":set -XTemplateHsaskell"
    --   exec g ":load test/Asd.hs"
    --   -- exec g "import Asd"
    --   -- exec g ":t xxxD" `shouldReturn` ["123"]
    --   exec g "xxx" `shouldReturn` ["123"]
    --   exec g "xxx2" `shouldReturn` ["34"]
    --   -- return ()
    --   -- print 1
    specify "POC: be able to reuse assemble in identA DecsQ of inj*" $ \g -> do
      T.writeFile "tmp/Scenarios/POCAssemble.hs" [text|
        import DI
        import Language.Haskell.TH
        inj
        a = 1
      |]
      loadModule' g "Scenarios/POCAssemble"
      execAssert g [qx| $(assemble $(varE $ mkName "aD")) |] (`shouldBeStr` "1\n")

    specify ">>= support" $ \g -> do
      T.writeFile "tmp/Scenarios/IO.txt" [text|123|]
      -- T.writeFile "tmp/Scenarios/IO.hs" [text|
      --   {-# language NoMonomorphismRestriction #-}
      --   module Scenarios.IO where

      --   import DI
      --   import Data.Time

      --   injG
      --   startupTime :: UTCTime
      --   startupTimeI = do
      --     print "startupTime init"
      --     getCurrentTime

      --   injG
      --   -- startupTimeString :: String
      --   startupTimeStringI startupTimeIO = show (startupTime :: UTCTime)
      -- |]

      T.writeFile "tmp/Scenarios/IO.hs" [text|
        {-# language NoMonomorphismRestriction #-}
        import DI

        injMG
        configI = do
          -- print "config init"
          readFile "tmp/Scenarios/IO.txt"

        injG
        fooSetting :: Int
        fooSettingI config = read config :: Int
      |]
      loadModule' g "Scenarios/IO"
      -- [ab| execAssert g "fooSetting" (`shouldBeStr` "123") |]
      [ab| execAssert g "$(assemble fooSettingD)" (`shouldBeStr` "123\n") |]

      -- describe "SO 232" $ do
      --   specify "SO 232" $ \g -> do
      --     -- 1 `shouldBe` 2
      --     [ab| 1 `shouldBe` 2 |]

      --   [aa| \_ -> ("1" `shouldBeStr` "2") |]


loadModule' g modName = do
  result <- exec g $ ":load tmp/" ++ modName ++ ".hs"
  -- if result $> last $> ("Ok, modules loaded" `isPrefixOf`)
  if result $> any ("Ok, modules loaded" `isPrefixOf`)
    -- then map printForward result $> sequence_
    then return ()
    else error $ "\n" ++ unlines result

specify a = if (any (`isPrefixOf` a) runOnlyPrefix)
  then Hspec.specify a
  else (\_->return ())
it a = if (any (`isPrefixOf` a) runOnlyPrefix)
  then Hspec.specify a
  else (\_->return ())

pd = parseDecs .> fromRight
pp = parsePat .> fromRight
pe = parseExp .> fromRight
-- fromRight (Right a) = a


printForward = (prefix ++) .> putStrLn
prefix = "  "

xcontext n _ = context n $ it "xcontext" pending
xit n _ = it n pending
xspecify n _ = specify n pending

displayLoadingInfo = id
  .> map (\case
        -- l@(Loading _ _) -> l $> show $> (prefix ++) $> putStrLn
        -- (Message{loadMessage=msg}) -> putStrLn $ unlines $ map (prefix ++) msg
        (Message{loadMessage=msg, loadSeverity=Error}) -> error $ ("\n" ++) $ unlines $ map (prefix ++) msg
        (Message{loadMessage=msg, loadSeverity=Warning}) -> error $ ("\n" ++) $ unlines $ map (prefix ++) msg
        _ -> return ()
      )
  .> sequence_
  where
  prefix = "  "


-- setUpGhcidCached = do
--   (g, ls) <- startGhci "stack ghci hs-di:exe:hs-di-cases" (Just ".") (const $ const (return ()))
--   displayLoadingInfo ls
--   return g

-- ghcid = unsafePerformIO $ newIORef Nothing
-- ghcid = unsafePerformIO $ newMVar Nothing

setUpGhcidUncached = do
  (g, ls) <- startGhci "stack ghci hs-di:exe:hs-di-cases" (Just ".") (const $ const (return ()))
  displayLoadingInfo ls
  return g

setUpGhcidCached = {-Hspec.runIO $-} do
  (lookupStore 0 $> handle) >>= \case
    Nothing -> do
      g <- setUpGhcidUncached
      (newStore g >> return ()) `catch` (\(e :: SomeException) -> print (2, e))
      -- writeIORef ghcid $ Just g
      return g
    Just s  -> do
      g <- readStore s
      reload g
      return g

  where
  handle = (`catch` f)
  f (e :: SomeException) = do
    print (1, e)
    return Nothing
  -- [x] TODO Handle:
  --     Test suite failure for package hs-di-0.2.2
  --         hs-di-test:  exited with: ExitFailure (-11)
  --     Logs printed to console

shouldBeStr :: String -> String -> IO ()
actual `shouldBeStr` expected
  | actual == expected = return ()
  | otherwise = expectationFailure failMsg
  where
    failMsg =
      [qx|
        Expected: {singleLineOrIndent expected}
        Actual:   {singleLineOrIndent actual}
      |]
    -- failMsg = T.unpack
    --   [text|
    --     $(T.pack expected)
    --     ${T.pack actual}
    --   |]
  -- (actual `shouldReturn` expected) `catch` (\(e :: SomeException) ->
  --   putStrLn actual
  -- )

singleLineOrIndent = f
  where
  f str@(isMultiline -> True) = "\n" <> indent 2 str
  f str = str

isMultiline = ('\n' `elem`)
indent n str =
  str
  $> lines
  $> map (replicate n ' ' ++)
  $> unlines

removeInteractive = id
  .> groupByIndentation
  .> filter (concat .> ("<interactive>:" `isPrefixOf`) .> not)
  .> concat

exec' g cmd = do
  stdoutB <- newIORef []
  stderrB <- newIORef []
  let
    f Stdout str = modifyIORef stdoutB (<> [str])
    f Stderr str = modifyIORef stderrB (<> [str])
  execStream g cmd f
  return (,) <*> readIORef stdoutB <*> readIORef stderrB

execAssert g cmd assert = exec g cmd
  >>= (\full -> full $> removeInteractive $> unlines $> assert
    $> failDetails ("Full: " <>  (full $> unlines $> singleLineOrIndent)))

parseTime' = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" .> fromJust
