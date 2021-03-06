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

instance Coarbitrary Bool where
    coarbitrary b = variant $ if b then 0 else 1

instance Coarbitrary Int where
    coarbitrary = variant

instance (Coarbitrary a, Coarbitrary b) => Coarbitrary (a, b) where
    coarbitrary (a, b) = coarbitrary a . coarbitrary b

instance Coarbitrary a => Coarbitrary [a] where
    coarbitrary as = foldl (.) id (coarbitrary <$> as)

instance (Arbitrary a, Coarbitrary b) => Coarbitrary (a -> b) where
    coarbitrary f gen = do
        a <- arbitrary
        coarbitrary (f a) gen

newtype Property = Prop (Gen Result)

unprop :: Property -> Gen Result
unprop (Prop x) = x

data Result = Result
    { ok :: Maybe Bool
    , stamp :: [String]
    , arguments :: [String]
    }

nothing :: Result
nothing = Result { ok = Nothing, stamp = [], arguments = [] }

result :: Result -> Property
result = Prop . pure

class Testable a where
    property :: a -> Property

instance Testable Bool where
    property b = result $ nothing { ok = Just b }

instance Testable Property where
    property = id

instance (Arbitrary a, Show a, Testable b) => Testable (a -> b) where
    property f = Prop $ do
            a <- arbitrary
            evaluate $ f a

evaluate :: Testable a => a -> Gen Result
evaluate = unprop . property

forAll :: (Show a, Testable b) => Gen a -> (a -> b) -> Property
forAll gen f = Prop $ do
    a <- gen
    res <- evaluate $ f a
    return (arg a res)
    where
        arg a res = res { arguments = show a : arguments res }

(==>) :: Testable a => Bool -> a -> Property
False ==> a = result nothing
True ==> a = property a

label :: Testable a => String -> a -> Property
label s a = Prop $ addArg <$> evaluate a
    where addArg res = res { stamp = s : stamp res }

classify :: Testable a => Bool -> String -> a -> Property
classify True name = label name
classify False _ = property

collect :: (Show a, Testable b) => a -> b -> Property
collect a = label $ show a

quickCheck :: (Testable a) => a -> IO ()
