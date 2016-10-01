{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Common
  ( module Common
  ) where

import ComposeLTR as Common

import Text.InterpolatedString.Perl6 as Common
import Language.Haskell.TH.Quote
import Data.String.Interpolate.Util

qx = QuasiQuoter{quoteExp = unindent .> quoteExp qc}
