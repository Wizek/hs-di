# Haskell Dependency Injection

A promising Dependency Injection system for Haskell.

# Why

The main motivation behind this project is to make it very easy to mock dependencies of functions for unit testing, even if they are nested many levels deep.

Another motivation of mine was to find a technique that works entirely at compile time, having the following benefits:

- compile-time type checking of all dependencies and wether they fit together
- no run-time performance penalty
- no run-time Dependency Injection related errors

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

*Note: `noun` is being overwritten while we are testing `statement`, which is not an immediate dependency but rather at the depth of 2.*

# How

In this project I am trying to emulate the manual assembly
of deeply nested and injected dependencies with the help of TemplateHaskell
and config compile-time dependency graphs.

To go into more details, this is what happens behind the scenes in the above example:

```haskell
Lib.hs:8:1-3: Splicing declarations
    inj
  ======>
    nounD = Dep "noun" []
    nounT = (noun)
    nounA = noun
    nounI = noun
Lib.hs:11:1-3: Splicing declarations
    inj
  ======>
    sentenceD = Dep "sentence" [nounD]
    sentenceT = (sentence, nounT)
    sentenceA = sentence nounA
    sentenceI = sentence
Lib.hs:14:1-3: Splicing declarations
    inj
  ======>
    statementD = Dep "statement" [sentenceD]
    statementT = (statement, sentenceT)
    statementA = statement sentenceA
    statementI = statement
```
```haskell
Spec.hs:7:1-3: Splicing declarations
    inj
  ======>
    nounMockD = Dep "nounMock" []
    nounMockT = (nounMock)
    nounMockA = nounMock
    nounMockI = nounMock
Spec.hs:11:5-23: Splicing expression
    assemble statementD
  ======>
    let (statement, (sentence, noun)) = statementT
    in statement (sentence noun)
Spec.hs:12:5-54: Splicing expression
    assemble $ override "noun" "nounMock" $ statementD
  ======>
    let (statement, (sentence, _)) = statementT
    in statement (sentence nounMock)
```

A couple things to note:

- You may be wondering what the suffix letters mean in the declarations.
  
  You don't have to concern yourself with them, it's part of the internal hidden API of the DI framework by design.
  
  (If you are curious however, they stand for "Dependency definitions/`Defs`", "Tuple", "Assembled", and "Injectable", respectively.)
- As you can see, at the end of the day, all this machinery achieves pretty much the same what a developer would do by hand: `statement (sentence noun)`
  
  The beauty, however, is that this doesn't have to be done by hand, as it would become immensly tideous and time-consuming as soon as we start to handle more than a couple dependencies.
- Mocking is equally elegant:
  
  `let (statement, (sentence, _)) = statementT in statement (sentence nounMock)` 
  
  (translated from `$(assemble $ override "noun" "nounMock" $ statementD)`)


## To try

To execute and experiment with modifying the above example:

```shell
git clone git@github.com:Wizek/hs-di.git
cd hs-di/examples/simple
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
