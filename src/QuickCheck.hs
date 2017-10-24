module QuickCheck where

import System.Random
import Control.Monad (ap, liftM)

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
