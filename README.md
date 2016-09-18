# Haskell Dependency Injection

A promising Dependency Injection system for Haskell.

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
  The beauty, however, is that this doesn't have to be done by hand, as it would become immensly tideous and time-consuming as soon as we start to handle more than a couple dependencies.  
- Mocking is equally elegant:  
  `let (statement, (sentence, _)) = statementT in statement (sentence nounMock)`  
  (translated from `$(assemble $ override "noun" "nounMock" $ statementD)`)

See more advanced example below.

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

*source: https://github.com/Wizek/hs-di/blob/v0.2.1/test/NotSoEasyToTestCode.hs#L17-L26* 

```haskell
timer <- $(makeTimerD
    $> override "putStrLn" "putStrLnMock"
    $> override "getCurrentTime" "getCurrentTimeMock"
    $> assemble
  )

readMockConsole `shouldReturn` []

writeIORef cTime $ parseTime "2016-01-01 14:00:00"
timer
readMockConsole `shouldReturn` ["2016-01-01 14:00:00 UTC"]

writeIORef cTime $ parseTime "2016-01-01 14:00:01"
timer
readMockConsole `shouldReturn`
  ["2016-01-01 14:00:00 UTC", "2016-01-01 14:00:01 UTC, diff: 1s"]
```

*excerpt from: https://github.com/Wizek/hs-di/blob/v0.2.1/test/MainSpec.hs#L95-L149*

### Pros and cons of this approach

  - `(+)` Supports values to be injected
  - `(+)` Supports functions to be injected
  - `(+2)` Supports overriding of arbitrary number and depth of dependencies
  - `(+2)` Compile time type checking (despites strings being used, those too are checked)
  - `(+)` Supports type variables
  - `(+)` Theoretically also supports surgically only overriding some subsets of dependencies
  - `(+)` Emulates how a human would do DI by hand, and does the hard work automatically
  - `(+)` Some module support
    - `(-.5)` The module support is not yet fully perfect
    - `(-.5)` Due to limitations of Template Haskell declaration splices, "variable not in scope" errors can pop up that are annoying. Although it is in theory possible to work around these, and it is planned for a later release.
  - `(?)` How is performance impacted? Does GHC notice `f (g x) (g x)`?

### Todo checklist

- [x] make multiple arguments work
- [x] Simplify Deps
- [x] reorder arguments of override
- [x] try with some real-life code
- [x] Write quasi quoter or TH splicer that writes the `Deps` definitions too
- [x] look for a way to have full module support (without having to explicitly re-export and risk name-clashes)
- [ ] Support function headers that are not immediately below
  - [ ] Consider using haskell-source-meta to extract parameter info 
- [ ] work around "variable not in scope" error by collecting all declarations in a splice at the end of the file
- [ ] have GHC support Dec TH splices in let bindings: https://ghc.haskell.org/trac/ghc/ticket/9880#comment:7
      Which could make overriding dependencies with mocks more pleasant
- [ ] have GHC lift stage restriction
