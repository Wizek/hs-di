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
      $( bD $> assemble ) `shouldBe` 2
      $( bD $> override "a" "aMock" $> assemble ) `shouldBe` 34
      $( dD $> override "a" "aMock" $> assemble ) `shouldBe` 37
      G.aA `shouldBe` 1

runOnlyPrefix = [""]
specify a = if (any (`isPrefixOf` a) runOnlyPrefix)
  then Hspec.specify a
  else (\_->return ())
