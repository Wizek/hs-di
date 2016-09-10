# Haskell Dependency Injection

A promising Dependency Injection system for Haskell.

# Why

The main motivation behind this project is to make it very easy to mock dependencies of functions for unit testing, even if they are nested many levels deep.

# Example

A motivating example:

```haskell
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
```

```haskell
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
```

Which when executed should output:

```
OK "Hello World!"
OK "Hello Dear Reader!"
```


# How

In this project I am trying to emulate the manual assembly
of deeply nested and injected dependencies with the help of TemplateHaskell
and config compile-time dependency graphs.

## To install

To try it out, you may run its tests:

```shell
git clone git@github.com:Wizek/hs-di.git
cd hs-di
stack test
```

### Pros and cons of this approach

  - `(+)` Supports values to be injected
  - `(+)` Supports functions to be injected
  - `(++)` Supports overriding of arbitrary number and depth of dependencies
  - `(++)` Compile time type checking (despites strings being used, those too are checked)
  - `(+)` Supports type variables
  - `(+)` Theoretically also supports surgically only overriding some subsets of dependencies

  - `(+)` Emulates how a human would do DI by hand, and does the hard work automatically

  - `(+)` Some module support

    - `(-0.5)` The module support is not yet fully perfect

  - `(?)` How is performance impacted? Does GHC notice `f (g x) (g x)`?

### Todos

- [x] TODO: make multiple argumetns work
- [x] TODO: Simplify Deps
- [x] TODO: reorder arguments of override
- [x] TODO: try with some real-life code
- [x] TODO: Write quasi quoter or TH splicer that writes the `Deps` definitions too
- [x] TODO: look for a way to have full module support (without having to explicitly re-export and risk name-clashes)
- [ ] TODO: have GHC support Dec TH splices in let bindings: https://ghc.haskell.org/trac/ghc/ticket/9880#comment:7
      Which could make overriding dependencies with mocks more pleasant
- [ ] TODO: have GHC lift stage restriction
