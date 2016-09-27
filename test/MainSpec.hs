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
import Test.HUnit.Lang (HUnitFailure(..))

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


  specify "code that is more real-life" $ do

    let parseTime t =
          fromJust $ parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" t
    mockConsole <- newIORef []
    cTime <- newIORef $ parseTime "2016-01-01 14:00:00"
    let
      -- $(inj)
      putStrLnMockD = dep "putStrLnMock" []
      putStrLnMockT = putStrLnMock
      putStrLnMock a = modifyIORef mockConsole (a :)

      -- readMockConsole = readIORef mockConsole >>= fmap reverse
      readMockConsole = do
        readIORef mockConsole >>= (reverse .> return)
        -- readIORef mockConsole $> (fmap reverse)

      -- $(inj)
      getCurrentTimeMockD = dep "getCurrentTimeMock" []
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


    context "ghcid" $ beforeAll setUpGhcid $ do

      it "ghcid test 1" $ \g -> do
        exec g "1+2" `shouldReturn` ["3"]

      it "ghcid test 2" $ \g -> do
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

        T.writeFile "test/Scenarios/CommonDependency.hs" [text|
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
        T.writeFile "test/Scenarios/CommonDependency2.hs" [text|
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
        T.writeFile "test/Scenarios/injAll.hs" [text|
          import DI

          aI = 1
          bI a = a + 2

          injAllG
        |]
        loadModule' g "Scenarios/injAll"
        execAssert g "$(assemble bD)" (`shouldBeStr` unlines ["3"])

      specify "injAllG out-of-order" $ \g -> do
        T.writeFile "test/Scenarios/injAll2.hs" [text|
          import DI

          bI a = a + 2
          aI = 1
          cI b = b + 3

          injAllG
        |]
        loadModule' g "Scenarios/injAll2"
        execAssert g "$(assemble cD)" (`shouldBeStr` unlines ["6"])

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

loadModule' g modName = do
  result <- exec g $ ":load test/" ++ modName ++ ".hs"
  -- if result $> last $> ("Ok, modules loaded" `isPrefixOf`)
  if result $> any ("Ok, modules loaded" `isPrefixOf`)
    -- then map printForward result $> sequence_
    then return ()
    else error $ "\n" ++ unlines result

-- runOnlyPrefix = ["!"]
-- runOnlyPrefix = ["unindent"]
runOnlyPrefix = [""]
specify a = if (any (`isPrefixOf` a) runOnlyPrefix)
  then Hspec.specify a
  else (\_->return ())
it a = if (any (`isPrefixOf` a) runOnlyPrefix)
  then Hspec.specify a
  else (\_->return ())

pd = parseDecs .> fromRight
pp = parsePat .> fromRight
pe = parseExp .> fromRight
fromRight (Right a) = a


printForward = (prefix ++) .> putStrLn
prefix = "  "

noop :: Monad m => m ()
noop = return ()
dont _ = noop

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
    Just s  -> readStore s
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
        Actual:   {singleLineOrIndent actual}
        Expected: {singleLineOrIndent expected}
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

failDetails details assert = do
  assert `catch` \(HUnitFailure loc msg) -> do
    throw $ HUnitFailure loc $ msg ++ "\n" ++ details

execAssert g cmd assert = exec g cmd
  >>= (\full -> full $> removeInteractive $> unlines $> assert
    $> failDetails ("Full: " <>  (full $> unlines $> singleLineOrIndent)))
