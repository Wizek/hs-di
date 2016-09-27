{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Assert (aa, ab) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta
import ComposeLTR
import Data.String.Utils
import Data.Monoid
import Data.String.Interpolate.Util
-- import Language.Haskell.QuasiQuotes

aa :: QuasiQuoter
aa = QuasiQuoter { quoteExp = f }
  where
  f :: String -> Q Exp
  f ss = do
    [| $(n "specify") $(return $ LitE $ StringL s) $ $(parseExp s $> either error return) |]
    where
    s = strip ss


ab :: QuasiQuoter
ab = QuasiQuoter { quoteExp = f }
  where
  f :: String -> Q Exp
  f ss = [| $(parseExp s $> either error return) $> $(n "failDetails") ("\nAssert: " <> $(n "singleLineOrIndent") $(stringE s)) |]
    where
    s = strip $ unindent ss

n s = return $ VarE $ mkName s
