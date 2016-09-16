{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Assert (aa) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta
import ComposeLTR
import Data.String.Utils
-- import Language.Haskell.QuasiQuotes

aa :: QuasiQuoter
aa = QuasiQuoter { quoteExp = f }

f :: String -> Q Exp
f ss = [| $(n "specify") $(return $ LitE $ StringL s) $ $(parseExp s $> either error return) |]
  where
  s = strip ss

n s = return $ VarE $ mkName s
