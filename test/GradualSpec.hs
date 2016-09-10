module GradualSpec where

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
import Gradual as G

inj
aMock = 33

spec = do
  describe "" $ do
    specify "dependencies" $ do
      removeIname "aI" `shouldBe` "a"
      removeIname "aasdasdI" `shouldBe` "aasdasd"
      (evaluate $ removeIname "a") `shouldThrow` anyException
      (evaluate $ removeIname "I") `shouldThrow` anyException

    specify "injG should allow for seamless introduction" $ do
      G.a `shouldBe` 1
      G.b `shouldBe` 2
      G.c `shouldBe` 3
      $( bD $> assembleG ) `shouldBe` 2
      $( bD $> override "aI" "aMock" $> assembleG ) `shouldBe` 34
      $( dD $> override "aI" "aMock" $> assembleG ) `shouldBe` 37
    --   $(assemble testIdiomaticModuleD) `shouldBe` 23
    --   $( testIdiomaticModuleD
    --     $> override "testIdimoaticImport" "testIdiomaticImportMock"
    --     $> assemble) `shouldBe` 48

-- runOnlyPrefix = ["!"]
runOnlyPrefix = [""]
specify a = if (any (`isPrefixOf` a) runOnlyPrefix)
  then Hspec.specify a
  else (\_->return ())
