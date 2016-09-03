-- {-# ghc_options -fdefer-type-errors #-}
{-# language ViewPatterns #-}
{-# language PatternSynonyms #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
import Data.Dynamic
import Data.Maybe
import qualified Data.Map as Map

pattern Dyn x <- (fromDynamic -> Just x)
  where Dyn x =  toDyn x


class A a where
    applyMany :: a -> [Dynamic] -> Dynamic

instance A Dynamic where
    applyMany xf [] = xf
    -- applyMany xf a = applyMany (fromDynamic xf $> fromJust :: Dynamic -> a) a
    applyMany xf a = error $ show a

-- instance A (Integer, String) where
--     applyMany x [] = Dyn x



-- instance A a => A (Integer -> a) where
--     applyMany f (x: xs) = applyMany (f $ fromJust $ fromDynamic x) xs

-- instance A a => A (String -> a) where
--     applyMany f (x: xs) = applyMany (f $ fromJust $ fromDynamic x) xs

instance A a => A (Dynamic -> a) where
    applyMany f (x: xs) = applyMany (f x) xs


-- f i s = (i, s) :: (Integer, String)


-- res :: Maybe (Integer, String)
-- res = fromDynamic $ applyMany f [Dyn (1 :: Integer), Dyn "asdf"]

f2 (Dyn i) (Dyn s) = Dyn ((i, s) :: (Integer, String))
res2 :: Maybe (Integer, String)
res2 = fromDynamic $ applyMany f2 [Dyn (1 :: Integer), Dyn "asdf"]


-- args = [toDyn 1, toDyn "a"]
-- -- f i s = (i:: Integer,s :: String)

-- res2 :: Maybe (Integer, String)
-- res2 = fromDynamic $ applyMany f args

($>) = flip ($)
(.>) = flip (.)

type Defs = Map.Map String ([String], Dynamic)

defs :: Defs
defs =
  Map.fromList
    [ ("a", (["b"], Dyn (\(Dyn b) -> Dyn ((b :: Integer) + 1))))
    , ("b", ([], Dyn $ Dyn (3 :: Integer)))
    -- , ("c", ([], Dyn (\b -> ((b :: Integer) + 1))))

    , ("f2", (["i1", "s1"], Dyn f2))
    , ("i1", ([], Dyn (1 :: Integer)))
    , ("s1", ([], Dyn "asdf"))
    ]

-- exec = exec' .> fromDynamic

exec' :: Defs -> String -> Dynamic
exec' defs k = asd (dynF) (map (exec' defs) depNames)
  where
  (depNames, dynF) = defs Map.! k

main = do
  -- print $ (exec defs "b" :: Maybe Integer)
  print $ (exec' defs "a" $> fromDynamic :: Maybe Integer)


type D = Dynamic

-- map ($ (toDyn ((2,3) :: (Int, Int), (*) :: Int -> Int->Int))) [((fromDynamic :: Dynamic -> Maybe ((Int, Int), Int -> Int -> Int)) .> fmap (\((a,b), f) -> f a b)), ((fromDynamic :: Dynamic -> Maybe (Int, Int -> Int)) .> fmap (\((a), f)-> f a ))] :: [Maybe Int]
asd dyn args = map ($ dyn)
  [ ((fromDynamic :: D -> Maybe (D)) .> fmap (\v -> v))
  -- , ((fromDynamic :: D -> Maybe (a)) .> fmap (\v -> v))
  , ((fromDynamic :: D -> Maybe (D -> D)) .> fmap (\f -> applyMany f args))
  , ((fromDynamic :: D -> Maybe (D -> D -> D)) .> fmap (\f -> applyMany f args))
  ]
  $> filter isJust
  $> (\case
    (Just a : _) -> a
    ([]) ->         error $ show (dyn, args)
  )