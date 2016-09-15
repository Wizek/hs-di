{-# LANGUAGE ViewPatterns #-}

-- foo
--   bar @ gggg
--   baz @ asdasdasdsd
--   =
--   [a|1|]

-- foo
--   (asd -> x)
--   baz @ asdasdasdsd
--   =
--   1

foo
  baz @ asdasdasdsd @ asd
  =
  1
