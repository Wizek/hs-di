# Haskell Dependency Injection

[Main10.hs](Main10.hs) is the most promising at the moment.
In fact, apart from some known issues, it seems to work quite well.
Much much better than the previous 9 or so approaches I tried.

To run:

```shell
git clone git@github.com:Wizek/exp-di-poc.git
cd exp-di-poc
runghc Main10
```

## From [Main10.hs](Main10.hs):

In this experiment I am trying to emulate the manual assembly
of deeply nested and injected dependencies with the help of TH
and config ADTs

### Pros and cons of this approach

  - `(+)` Supports values to be injected
  - `(+)` Supports functions to be injected
  - `(++)` Supports overriding of arbitrary number and depth of dependencies
  - `(++)` Compile time type checking (despites strings being used, those too are checked)
  - `(+)` Supports type variables 
  - `(+)` Theoretically also supports surgically only overriding some subsets of dependencies

  - `(+)` Emulates how a human would do DI by hand, and does the hard work automatically

  - `(-)` Limited module support, requires re-exporting
    - `(?)` Could it be possible to pass the default dependencies along in `Deps` somehow?
      That could get rid of this issue
    - `(+)` At least it supports qualified names:
      
      ```
      *Main> $(assemble $ Dep "Prelude.id" []) 1
      1
      *Main> $(assemble $ Dep "Prelude.*" []) 2 3
      6
      ```

  - `(?)` How is performance impacted? Does GHC notice `f (g x) (g x)`? 

### Todos

- [x] TODO: make multiple argumetns work
- [x] TODO: Simplify Deps
- [x] TODO: reorder arguments of override
- [x] TODO: try with some real-life code
- [ ] TODO: Write quasi quoter or TH splicer that writes the `Deps` definitions too
- [ ] TODO: look for a way to have full module support (without having to explicitly re-export and risk name-clashes)
