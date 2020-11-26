module Util where

clamp :: Double -> Double -> Double -> Double
clamp lo hi n
  | n < lo = lo
  | n > hi = hi
  | otherwise = n

inf = 1/0 :: Double