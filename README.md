# Haskell Dependency Injection

_A promising Dependency Injection system for Haskell._

[![License](https://img.shields.io/github/license/Wizek/hs-di.svg?style=flat-square)](https://github.com/Wizek/hs-di/blob/master/LICENSE)
[![Build Status](https://img.shields.io/travis/Wizek/hs-di.svg?style=flat-square)](https://travis-ci.org/Wizek/hs-di)
[![Hackage](https://img.shields.io/hackage/v/hs-di.svg?style=flat-square)](https://hackage.haskell.org/package/hs-di)

# Why

The main motivation behind this project is to make it very easy to mock dependencies of functions for unit testing, even if they are nested many levels deep.

Another motivation of mine was to find a technique that works entirely at compile time, having the following benefits:

- compile-time type checking of all dependencies and whether they fit together
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

*Observe*: In the second assertion, `noun` is being overridden while we are testing `statement`. `noun` is not an immediate dependency of `statement` but a dependency at 2 levels deep.

# How

In this project I am trying to emulate the manual assembly
of deeply nested and injected dependencies with the help of TemplateHaskell
and compile-time dependency graphs as configuration.

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
  (If you are curious however, they stand for "Dependency definitions/`Deps`", "Tuple", "Assembled", and "Injectable", respectively.)  
- As you can see, at the end of the day, all this machinery achieves pretty much the same what a developer would do by hand: `statement (sentence noun)`  
  The beauty, however, is that this doesn't have to be done by hand, as it would become immensely tedious and time-consuming as soon as we start to handle more than a couple dependencies.  
- Mocking is equally elegant:  
  `let (statement, (sentence, _)) = statementT in statement (sentence nounMock)`  
  (translated from `$(assemble $ override "noun" "nounMock" $ statementD)`)

See a more advanced example below.

## To try

To execute the above example:

```shell
git clone git@github.com:Wizek/hs-di.git
cd hs-di/examples/simple
stack test
```

You may also experiment with modifying the files in `hs-di/examples/simple` then re-running `stack test` to get an intuitive understanding of how this library works.

## More advanced example

While the following code may not be the most elegant or useful, it at least shows the power of dependency injection when it comes to mocking and testing IO code that deals with `putStrLn` and `getCurrentTime` in a fully deterministic way.

```haskell
inj
makeTimer putStrLn getCurrentTime = liftIO $ do
  prevTime <- newIORef Nothing
  return $ liftIO $ do
    pTime <- readIORef prevTime
    time <- getCurrentTime
    writeIORef prevTime $ Just time
    case pTime of
      Nothing -> putStrLn $ show time
      Just a  -> putStrLn $ show time ++ ", diff: " ++ (show $ diffUTCTime time a)
```

*(source: https://github.com/Wizek/hs-di/blob/v0.2.1/test/NotSoEasyToTestCode.hs#L17-L26)* 

```haskell
mockConsole <- newIORef []
cTime <- newIORef $ parseTime' "2000-01-01 00:00:00"
let
  putStrLnMock a = modifyIORef mockConsole (a :)
  getCurrentTimeMock = readIORef cTime
  readMockConsole = readIORef mockConsole >>= reverse .> return

timer <- $(makeTimerD
  $> override "putStrLn" "putStrLnMock"
  $> override "getCurrentTime" "getCurrentTimeMock"
  $> assemble)

readMockConsole `shouldReturn` []

timer
readMockConsole `shouldReturn` ["2000-01-01 00:00:00 UTC"]

writeIORef cTime $ parseTime' "2000-01-01 00:00:00.0001"
timer
readMockConsole `shouldReturn`
  ["2000-01-01 00:00:00 UTC", "2000-01-01 00:00:00.0001 UTC, diff: 0.0001s"]
```

*(source: https://github.com/Wizek/hs-di/blob/9fd5e4/test/MainSpec.hs#L135-L156)*

### Pros and cons of this approach

  - `(+)` Supports values to be injected
  - `(+)` Supports functions to be injected
  - `(+2)` Supports overriding of arbitrary number and depth of dependencies
  - `(+2)` Compile time type checking (despites strings being used, those too are checked at compile time)
  - `(+)` Supports type variables
  - `(+)` Theoretically also supports surgically only overriding some subsets of dependencies
  - `(+)` Emulates how a human would do DI by hand, and does the hard work automatically
  - `(+)` Modules are fully supported as of `v0.3+` 
  - `(?)` How is compile/startup performance impacted? Does GHC notice `f (g x) (g x)`?

### Inspirations

This package was initially inspired by the Dependency Injection framework of AngularJS (1.x).  
Additional inspiration came when I was looking for ways to make DI work in a statically typed language at compile time, and found out about Dagger (Java).

### Todo checklist

- [x] `v0.2+` make multiple arguments work
- [x] `v0.2+` Simplify Deps
- [x] `v0.2+` reorder arguments of override
- [x] `v0.2+` try with some real-life code
- [x] `v0.2+` Write quasi quoter or TH splicer that writes the `Deps` definitions too
- [x] `v0.2+` look for a way to have full module support (without having to explicitly re-export and risk name-clashes)
- [x] `v0.3+` Support function headers that are not immediately below
    - [ ] Consider using haskell-source-meta to extract parameter info 
- [x] `v0.3+` work around "variable not in scope" error by collecting all declarations in a splice at the end of the file
- [x] `v0.3+` Allow single dependency more than once
- [ ] have GHC support Dec TH splices in let bindings: https://ghc.haskell.org/trac/ghc/ticket/9880#comment:7
      Which could make overriding dependencies with mocks more pleasant
- [ ] have GHC lift stage restriction

### Experimental Features

#### "Inject Gradual": Gradually introduce DI


```haskell
-- Lib.hs
{-# language TemplateHaskell #-}

module Lib where

import DI

injG
nounI = "World"

injG
sentence :: String
sentenceI noun = "Hello " ++ noun

injG
statementI sentence = sentence ++ "!"

legacyStatement = sentence ++ "..."
```

The `injG` top level `Q [Dec]` splice requires the dependency name to end with the suffix `I`, and defines an injected (assembled) value without the suffix to be used in legacy code not yet part of dependency injection. E.g. `nounI` --> `noun`. This allows for more gradual transition of a codebase into using DI, since declarations can be updated one at a time while allowing the program to remain able to be compiled and identical in terms of execution and behaviour.

```
Lib.hs:11:1-3: Splicing declarations
    injG
  ======>
    sentenceD = Dep "sentence" [nounD]
    sentenceT = (sentenceI, nounT)
    sentenceA = sentenceI nounA
    sentence = sentenceA
```

#### Inject All

```haskell
-- Lib.hs
{-# language TemplateHaskell #-}

module Lib where

import DI

injAllG

sentenceI noun = "Hello " ++ noun
nounI = "World"
statementI sentence = sentence ++ "!"
```

This allows us to overcome multiple limitations of TemplateHaskell having to do with scoping, and avoid errors such as `Lib.hs:10:1: Not in scope: ‘noun’`.
It also allows us to define our declarations in arbitrary order.

```
Lib.hs:8:1-7: Splicing declarations
    injAllG
  ======>
    sentenceD = Dep "sentence" Original Pure [nounD] :: Deps
    sentenceT = (sentenceI, nounT)
    sentenceA = sentenceI noun
    sentence = sentenceA
    nounD = Dep "noun" Original Pure [] :: Deps
    nounT = (nounI)
    nounA = nounI
    noun = nounA
    statementD = Dep "statement" Original Pure [sentenceD] :: Deps
    statementT = (statementI, sentenceT)
    statementA = statementI sentence
    statement = statementA
```

Some further reading on the subject: http://stackoverflow.com/questions/20876147/haskell-template-haskell-and-the-scope

#### Override inline

This allows us to define short and ad-hoc mocks inline

```haskell
$(assemble $ override "noun" "\"there\"" $ statementD) `shouldBe` "Hello there!"
```

Alternatively, if one uses a HereDoc such as [`interpolatedstring-perl6`](https://hackage.haskell.org/package/interpolatedstring-perl6) dealing with quotation marks can be simpler:

```haskell
$(assemble $ override "noun" [qc|"there"|] $ statementD) `shouldBe` "Hello there!"
```

#### Monadic Inject, InjIO

```haskell
-- Lib.hs
module Lib where

import DI

injG
startupTimeI = getCurrentTime

injG
runI (InjIO startupTime) = do
  print startupTime
  print startupTime
  -- Prints the same startup time twice, only calls getCurrentTime once
```

```haskell
-- Main.hs
import Lib
import DI

main = do
  join $(assemble runD)
  -- `join` is necessary for now, but it is hoped to make it
  -- not required for a later release.
```

This is a highly experimental feature. It is intended to be useful in not just testing, but also to make it simpler to deal with monadic dependencies of initializing functions or your whole application. E.g. reading configuration from disk, making network requests, initializing a random seed without it having to be passed around, etc... All while still enabling the code to behave fully deterministically under testing.
