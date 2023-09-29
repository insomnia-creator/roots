module Main where

import Cryptol.Backend.FloatHelpers (BF (BF, bfExpWidth, bfPrecWidth, bfValue), fpCheckStatus, fpPP)
import Cryptol.TypeCheck.PP (PPFloatFormat (FloatFrac), PPOpts (PPOpts, useAscii, useBase, useFPBase, useFPFormat, useFieldOrder, useInfLength))
import Data.Int (Int64)
import LibBF
import System.TimeIt

by :: IO [Double]
by = fmap (map read . words) getLine

-- functions
-- f :: Double -> Double
-- fp :: Double -> Double
-- f x = x ** 10 + 5 * (x ** 9) + 3 * (x ** 2) + 18

-- BigFloat to the power of an Int64
xn :: BigFloat -> Int64 -> BigFloat
xn x n = fpCheckStatus (bfPow (float256 NearEven) x (bfFromInt n))

-- BigFloat times an integer
xy :: BigFloat -> Int64 -> BigFloat
xy x y = fpCheckStatus (bfMul (float256 NearEven) x (bfFromInt y))

-- Addition of two BigFloats
xpy :: BigFloat -> BigFloat -> BigFloat
xpy x y = fpCheckStatus (bfAdd (float256 NearEven) x y)

-- Subtraction of two BigFloats, x - y
xny :: BigFloat -> BigFloat -> BigFloat
xny x y = fpCheckStatus (bfSub (float256 NearEven) x y)

-- Division of two BigFloats x/y
xby :: BigFloat -> BigFloat -> BigFloat
xby x y = fpCheckStatus (bfDiv (float256 NearEven) x y)

-- x100 + 3x2 + 2x + 10
f :: BigFloat -> BigFloat
-- f x = x `xn` 3
-- f x = x `xn` 3
-- f x = xpy (xpy (xn x 87) (xy (xn x 2) 3)) (xpy (xy x 2) (bfFromInt 10))
-- f x = xpy (xpy (xn x 87) (xy (xn x 2) 3)) (xpy (xy x 2) (bfFromInt 10))
-- f x = xpy (xn x 9) (bfFromInt 512)
-- take an f(x) = x^5 - 15 x^4 + 85 x^3 - 225 x^2 + 274 x - 120
-- (6 terms)

-- f x = t5 `xny` t4 `xpy` t3 `xny` t2 `xpy` t1 `xny` t0
-- where
-- t5 = x `xn` 5
-- t4 = x `xn` 4 `xy` 15
-- t3 = x `xn` 3 `xy` 85
-- t2 = x `xn` 2 `xy` 225
-- t1 = x `xy` 274
-- t0 = bfFromInt 120

f x = t4 `xny` t3 `xpy` bfFromInt 2
  where
    t4 = x `xn` 4
    t3 = x `xn` 3 `xy` 3

-- definitely rooted
-- f x = x^10 - 45 x^9 + 870 x^8 - 9450 x^7 + 63273 x^6 - 269325 x^5 + 723680 x^4 - 1172700 x^3 + 1026576 x^2 - 362880 x
-- 5x4 + 6x + 2
f' :: BigFloat -> BigFloat
-- fp x = xpy (xpy (xy (xn x 86) 87) (xy x 6)) (bfFromInt 10)
-- fp x = xy (xn x 8) 9
-- x ^ 5 + 4x + 3
-- f' x = t5 `xny` t4 `xpy` t3 `xny` t2 `xpy` t1
-- where
-- t5 = x `xn` 4 `xy` 5
-- t4 = x `xn` 3 `xy` 60
-- t3 = x `xn` 2 `xy` 225
-- t2 = x `xy` 450
-- t1 = bfFromInt 274
f' x = x `xn` 3 `xy` 4 `xny` x `xn` 2 `xy` 9

--- x0 - (f (x0) / f'(x0))
approximate :: BigFloat -> BigFloat
-- approximate x0 = xny x0 (xby (f x0) (f' x0))
approximate x0 = x0 `xny` (f x0 `xby` f' x0)

main :: IO ()
main = do
  putStrLn "Enter an approximation and times to iterate"
  doubs <- by
  let approxDoub = head doubs
  let its = doubs !! 1
  let approx = bfFromDouble approxDoub
  let approximation = iterate approximate approx !! round its
  timeIt $ putStrLn ("Raw approximate: " ++ show (iterate approximate approx !! round its))
  let ops = PPOpts {useInfLength = 4, useFieldOrder = maxBound, useAscii = True, useBase = 10, useFPFormat = FloatFrac 256, useFPBase = 10}
  putStrLn "Pretty value:"
  let bf = BF {bfValue = approximation, bfPrecWidth = 256, bfExpWidth = 256}
  timeIt $ print (fpPP ops bf)
