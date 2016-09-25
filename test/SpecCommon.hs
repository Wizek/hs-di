{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module SpecCommon (module SpecCommon) where

import Common                         as SpecCommon
import Text.InterpolatedString.Perl6  as SpecCommon
import Language.Haskell.TH.Quote
import Data.String.Interpolate.Util

qx = QuasiQuoter{quoteExp = unindent .> quoteExp qc}
