{-# LANGUAGE ScopedTypeVariables #-}

import Data.Fixed.Q
import Numeric.Extras
import Test.Hspec
import Test.QuickCheck

import CORDIC

main :: IO ()
main = hspec $ do
    describe "CORDIC" $ do
      it "multiplication" $
        property prop_mul
      it "division" $
        property prop_div
      it "cos" $
        property prop_cos
      it "sin" $
        property prop_sin
      it "cosh" $
        property prop_cosh
      it "sinh" $
        property prop_sinh
      it "atan" $
        property prop_atan
      it "atanh" $
        property prop_atanh
      it "exp" $
        property prop_exp
      it "log" $
        property prop_log
      it "sqrt" $
        property prop_sqrt
      it "pow" $
        property prop_pow
      it "logBase" $
        property prop_logBase

mul' :: (Ord a, GenShift a) => a -> a -> a
mul' a b = y
  where
    (_, y, _) = cordic Linear Rotation (-2) n (a, 0, b)
    n = 64

div' :: (Ord a, GenShift a) => a -> a -> a
div' a b = z
  where
    (_, _, z) = cordic Linear Vectoring 0 n (b, a, 0)
    n = 64

cos' :: (Ord a, GenShift a) => a -> a
cos' rad = x
  where
    (x, _, _) = cordic Circular Rotation (-2) n (1/k Circular (-2) n, 0, rad)
    n = 64

sin' :: (Ord a, GenShift a) => a -> a
sin' rad = y
  where
    (_, y, _) = cordic Circular Rotation (-2) n (1/k Circular (-2) n, 0, rad)
    n = 64

cosh' :: (Ord a, GenShift a) => a -> a
cosh' rad = x
  where
    (x, _, _) = cordic (Hyperbolic 1) Rotation (-3) n (1/k (Hyperbolic 1) (-3) n, 0, rad)
    n = 64

sinh' :: (Ord a, GenShift a) => a -> a
sinh' rad = y
  where
    (_, y, _) = cordic (Hyperbolic 1) Rotation (-3) n (1/k (Hyperbolic 1) (-3) n, 0, rad)
    n = 64

atan2' :: (Ord a, GenShift a) => a -> a -> a
atan2' y x = theta
  where
    (_, _, theta) = cordic Circular Vectoring (-2) n (x, y, 0)
    n = 64

atan' :: (Ord a, GenShift a) => a -> a
atan' r = atan2' r 1

atanh2' :: (Ord a, GenShift a) => a -> a -> a
atanh2' y x = theta
  where
    (_, _, theta) = cordic (Hyperbolic 1) Vectoring (-3) n (x, y, 0)
    n = 64

atanh' :: (Ord a, GenShift a) => a -> a
atanh' r = atanh2' r 1

exp' :: (Ord a, GenShift a) => a -> a
exp' x = coshx + sinhx
  where
    (coshx, sinhx, _) = cordic (Hyperbolic 1) Rotation (-3) n (1/k (Hyperbolic 1) (-3) n, 0, x)
    n = 64

log' :: (Ord a, GenShift a) => a -> a
log' x = 2*theta
  where
    (_, _, theta) = cordic (Hyperbolic 1) Vectoring (-3) n (x+1, x-1, 0)
    n = 64

sqrt' :: forall a . (Ord a, Num a, GenShift a) => a -> a
sqrt' x = x'
  where
    (x', _, _) = cordic (Hyperbolic 1) Vectoring (-3) n (x+c, x-c, 0)
    n = 64
    c :: a
    c = 1 / (4 * (k (Hyperbolic 1) (-3) n) ** 2)

pow' :: (Ord a, GenShift a) => a -> a -> a
pow' b x = coshx + sinhx
  where
    (coshx, sinhx, _) = cordic (Hyperbolic (log b)) Rotation (-3) n (1/k (Hyperbolic 1) (-3) n, 0, x)
    n = 64

logBase' :: (Ord a, GenShift a) => a -> a -> a
logBase' b x = 2*theta
  where
    (_, _, theta) = cordic (Hyperbolic (log b)) Vectoring (-3) n (x+1, x-1, 0)
    n = 64

inRange :: (RealExtras a, Arbitrary a) => a -> Gen a
inRange r = fmod <$> arbitrary <*> pure r

posRange :: (RealExtras a, Arbitrary a) => a -> Gen a
posRange r = do
    Positive x <- arbitrary
    return (abs (fmod x r))

deps :: Double
deps = 1e-9

mkprop_equal :: Double
             -> Double
             -> (Double -> Double)
             -> (Double -> Double)
             -> Property
mkprop_equal eps r f g = forAll (inRange r) $ \x -> abs (f x - g x) < eps

lINEAR_MAX :: Double
lINEAR_MAX = lim Linear 0 100

cIRCROT_MAX :: Double
cIRCROT_MAX = lim Circular (-2) 100

hPERROT_MAX :: Double
hPERROT_MAX = lim (Hyperbolic 1) (-3) 100

prop_mul :: Double -> Property
prop_mul a = forAll (inRange lINEAR_MAX) $ \b -> abs (mul' a b - a*b) < deps

prop_div :: Positive Double -> Property
prop_div (Positive b) =
  forAll (posRange lINEAR_MAX) $
    \r -> b /= 0 && r /= 0 ==> abs (div' (b*r) b - r) < deps

prop_cos :: Property
prop_cos = mkprop_equal deps cIRCROT_MAX cos cos'

prop_sin :: Property
prop_sin = mkprop_equal deps cIRCROT_MAX sin sin'

prop_cosh :: Property
prop_cosh = mkprop_equal deps hPERROT_MAX cosh cosh'

prop_sinh :: Property
prop_sinh = mkprop_equal deps hPERROT_MAX sinh sinh'

prop_atan :: Property
prop_atan = mkprop_equal deps (tan cIRCROT_MAX) atan atan'

prop_atanh :: Property
prop_atanh = mkprop_equal deps (tanh hPERROT_MAX) atanh atanh'

prop_exp :: Property
prop_exp = mkprop_equal deps hPERROT_MAX exp exp'

prop_log :: Property
prop_log = forAll (posRange hPERROT_MAX) $ \x -> abs (log x - log' x) < deps

prop_sqrt :: Property
prop_sqrt = forAll (posRange hPERROT_MAX) $ \x -> x > 1e-1 ==> abs (sqrt x - sqrt' x) < 1e-8

prop_pow :: Positive Double -> Property
prop_pow (Positive b) = mkprop_equal deps (hPERROT_MAX / abs (log b)) (b **) (pow' b)

prop_logBase :: Positive Double -> Property
prop_logBase (Positive b) =
  b /= 1 ==> forAll (posRange (hPERROT_MAX / abs (log b))) $ \x ->
    abs (logBase b x - logBase' b x) < 1e-8
