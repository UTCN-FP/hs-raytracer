module Random (nextDouble, newRandom, Random) where

import qualified Data.Word as W
import Data.Bits

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