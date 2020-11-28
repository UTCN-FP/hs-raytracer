module Util.Random where

import qualified Data.Word as W
import Data.Bits

import Vec3

newtype Random = Random W.Word64

nextXor :: W.Word64 -> W.Word64
nextXor w =
  let a = w `xor` (w `shiftR` 12)
      b = a `xor` (a `shiftR` 25)
      c = b `xor` (b `shiftR` 27)
   in c * 0x2545F4914F6CDD1D

newRandom :: W.Word64 -> Random
newRandom seed = Random seed

nextDouble :: Random -> (Double, Random)
nextDouble (Random w) = (fromIntegral w / fromIntegral maxW, Random $ nextXor w) where maxW = maxBound :: W.Word64

nextDoubleRange :: Double -> Double -> Random -> (Double, Random)
nextDoubleRange lo hi (Random w) = (d, Random $ nextXor w) where
   maxW = maxBound :: W.Word64
   d = lo + (hi - lo) * (fromIntegral w / fromIntegral maxW)

randomVec :: Random -> (Vec3, Random)
randomVec seed = 
  let (vx, seed') = nextDouble seed
      (vy, seed'') = nextDouble seed'
      (vz, seed''') = nextDouble seed''
  in (Vec3 vx vy vz, seed''')

randomVecRange :: Double -> Double -> Random -> (Vec3, Random)
randomVecRange lo hi seed = 
  let (vx, seed') = nextDoubleRange lo hi seed
      (vy, seed'') = nextDoubleRange lo hi seed'
      (vz, seed''') = nextDoubleRange lo hi seed''
  in (Vec3 vx vy vz, seed''')

randomVecInSphere :: Random -> (Vec3, Random)
randomVecInSphere seed = head $ dropWhile (\(v, _) -> lenSqrd v < 1) $ iterate itFn start where
  unitRnd s = randomVecRange (-1) 1 s
  start = unitRnd seed
  itFn (_, s) = unitRnd s

randomVecInHemisphere :: Random -> Vec3 -> (Vec3, Random)
randomVecInHemisphere seed n = 
  if inUnitS `dot` n > 0 then
    (inUnitS, s)
  else
    (neg inUnitS, s)
  where (inUnitS, s) = randomVecInSphere seed 