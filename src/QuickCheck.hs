module QuickCheck where

import System.Random
import Control.Monad (ap, liftM, liftM2)

newtype Gen a = Gen (Int -> StdGen -> a)

choose :: Random a => (a, a) -> Gen a
choose bounds = Gen (\n r -> fst (randomR bounds r))

variant :: Int -> Gen a -> Gen a
variant v (Gen m) = Gen (\n r -> m n (rands r !! (v + 1)))
    where
        rands r0 = r1 : rands r2
            where
                (r1, r2) = split r0

promote :: (a -> Gen b) -> Gen (a -> b)
promote f = Gen (\n r -> \a ->
    let Gen m = f a in m n r)

sized :: (Int -> Gen a) -> Gen a
sized f = Gen (\n r ->
    let Gen m = f n in m n r)

instance Functor Gen where
    fmap = liftM

instance Applicative Gen where
    (<*>) = ap
    pure = return

instance Monad Gen where
    return a = Gen (\n r -> a)
    Gen m1 >>= k = Gen (\n r ->
        let
            (r1, r2) = split r
            Gen m2 = k (m1 n r1)
        in
            m2 n r2)

elements :: [a] -> Gen a
elements xs = (xs !!) <$> choose (0, length xs - 1)

oneof :: [Gen a] -> Gen a
oneof gs = elements gs >>= id

frequency :: [(Int, Gen a)] -> Gen a
frequency = oneof . (>>= \(i, g) -> replicate i g)

class Arbitrary a where
    arbitrary :: Gen a

instance Arbitrary Bool where
    arbitrary = elements [True, False]

instance Arbitrary Int where
    arbitrary = sized (\n -> choose (-n, n))

instance (Arbitrary a, Arbitrary b) => Arbitrary (a, b) where
    arbitrary = liftM2 (,) arbitrary arbitrary

instance Arbitrary a => Arbitrary [a] where
    arbitrary = do
        n <- arbitrary
        sequence $ replicate (abs n) arbitrary

instance (Coarbitrary a, Arbitrary b) => Arbitrary (a -> b) where
    arbitrary = promote (`coarbitrary` arbitrary)

class Coarbitrary a where
    coarbitrary :: a -> Gen b -> Gen b
