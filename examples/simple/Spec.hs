-- Spec.hs
{-# language TemplateHaskell #-}

import DI
import Lib

inj
nounMock = "Dear Reader"

main = do
  $(assemble statementD) `shouldBe` "Hello World!"
  $(assemble $ override "noun" "nounMock" $ statementD) `shouldBe` "Hello Dear Reader!"

-- assertion function
shouldBe = shouldBeF show
shouldBeF f actual expected | actual == expected = putStrLn $ "OK " ++ f actual
                            | otherwise          = error $ "FAIL " ++ f actual ++ " /= " ++ f expected
