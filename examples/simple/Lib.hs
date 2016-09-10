-- Lib.hs
{-# language TemplateHaskell #-}

module Lib where

import DI

inj
noun = "World"

inj
sentence noun = "Hello " ++ noun

inj
statement sentence = sentence ++ "!"
