{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where
import MCPrelude

type Gen a = Seed -> (a, Seed)
generalA :: (a -> b) -> Gen a -> Gen b
generalA f ga s0 = (f x, s1)
  where (x, s1) = ga s0

randEven :: Gen Integer
randEven = generalA (*2) rand

randOdd :: Gen Integer
randOdd = generalA (+1) randEven

randTen :: Gen Integer
randTen = generalA (*10) rand

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair ga gb s0 = ((x, y), s2)
  where (x, s1) = ga s0
        (y, s2) = gb s1

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f ga gb s0 = (f x y, s2)
  where (x, s1) = ga s0
        (y, s2) = gb s1

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 = generalB (,)

repRandom :: [Gen a] -> Gen [a]
repRandom [] s0 = ([], s0)
repRandom (g:gs) s0 = (x:xs, sf)
  where (x, s1) = g s0
        (xs, sf) = repRandom gs s1

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo ga f s0 = f x s1
  where (x, s1) = ga s0

mkGen :: a -> Gen a
mkGen x = \s0 -> (x, s0)

-- Product should be 8681089573064486461641871805074254223660
fiveRands :: [Integer]
fiveRands = [a, b, c, d, e]
  where (a, s1) = rand $ mkSeed 1
        (b, s2) = rand s1
        (c, s3) = rand s2
        (d, s4) = rand s3
        (e, _)  = rand s4

randLetter :: Seed -> (Char, Seed)
randLetter seed = (toLetter $ f `mod` 26, s)
  where (f, s) = rand seed

-- SHA should be 9d475eb78d3e38085220ed6ebde9d8f7d26540bb1c8f9382479c3acd4c8c94a3
randString3 :: String
randString3 = [a,b,c]
  where (a, s1) = randLetter $ mkSeed 1
        (b, s2) = randLetter s1
        (c, s3) = randLetter s2
