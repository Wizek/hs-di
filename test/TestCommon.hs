module TestCommon where

import Control.Exception
import Test.Hspec
import Data.Monoid
import Test.HUnit.Lang (HUnitFailure(..))

runOnlyPrefix =
  [""]
  -- ["!"]
  -- ["!!"]
  -- ["!!!"]
  -- ["!!!!"]
  -- ["!!!!!"]
  -- ["!!!!!!"]
  -- ["unindent"]
  -- ["SO"]

experimental = failDetails "\nExperimental feature, no big deal if it fails."
expPendingIfFails = failWith $ \e@(HUnitFailure loc str) ->
  pendingWith $ "Experimental:\n" <> show loc <> "'\n" <> str

failDetails details assert = do
  assert `catch` \(HUnitFailure loc msg) -> do
    throw $ HUnitFailure loc $ msg ++ "\n" ++ details

failWith with assert = do
  assert `catch` \(e :: HUnitFailure) -> with e

(==!) = shouldBe

dont = const noop
-- dont _ = noop

noop :: Monad m => m ()
noop = return ()
