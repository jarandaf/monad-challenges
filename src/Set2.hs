{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude

data Maybe a = Nothing | Just a

headMay :: [a] -> Maybe a
headMay []      = Nothing
headMay (x:xs)  = Just x

tailMay :: [a] -> Maybe [a]
tailMay []      = Nothing
tailMay (x:xs)  = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay k ((xa, xb):xs)
  | k == xa   = Just xb
  | otherwise = lookupMay k xs

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay x y = Just (x / y)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay []     = Nothing
maximumMay (x:xs) = Just (foldl max x xs)

minimumMay :: Ord a => [a] -> Maybe a
minimumMay []     = Nothing
minimumMay (x:xs) = Just (foldl min x xs)

queryGreek :: GreekData -> String -> Maybe Double
queryGreek d key = case lookupMay key d of
  Nothing -> Nothing
  Just xs -> case headMay xs of
    Nothing -> Nothing
    Just h -> case tailMay xs of
      Nothing -> Nothing
      Just t -> case maximumMay t of
        Nothing -> Nothing
        Just m -> divMay (fromIntegral m) (fromIntegral h)

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain _ Nothing   = Nothing
chain f (Just a)  = f a

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = flip chain

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 d key =
  lookupMay key d   `link` \vals ->
  headMay vals      `link` \h ->
  tailMay vals      `link` \t ->
  maximumMay t      `link` \m ->
  divMay (fromIntegral m) (fromIntegral h)

mkMaybe :: a -> Maybe a
mkMaybe = Just

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries salaries a b =
  lookupMay a salaries `link` \salA ->
  lookupMay b salaries `link` \salB ->
  mkMaybe (salA + salB)

yLink :: Maybe a -> Maybe b -> (a -> b -> c) -> Maybe c
yLink a b f =
  a `link` \a' ->
  b `link` \b' ->
  mkMaybe $ f a' b'

tailProd :: Num a => [a] -> Maybe a
tailProd xs = tailMay xs `link` (mkMaybe . product)

tailSum :: Num a => [a] -> Maybe a
tailSum xs = tailMay xs `link` (mkMaybe . sum)

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe f a =
  a `link` \a' ->
  mkMaybe $ f a'

tailProd2 :: Num a => [a] -> Maybe a
tailProd2 = transMaybe product . tailMay

tailSum2 :: Num a => [a] -> Maybe a
tailSum2 = transMaybe sum . tailMay

combine :: Maybe (Maybe a) -> Maybe a
combine Nothing   = Nothing
combine (Just x)  = x

tailMax :: Ord a => [a] -> Maybe a
tailMax = combine . transMaybe maximumMay . tailMay

tailMin :: Ord a => [a] -> Maybe a
tailMin = combine . transMaybe minimumMay . tailMay

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 salaries a b = yLink s1 s2 (+)
  where s1 = lookupMay a salaries
        s2 = lookupMay b salaries

instance Show a => Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just a) = "Just " ++ show a
