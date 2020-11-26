module Hit.Hit where

import qualified Sphere as S
import Ray

import Vec3

import Hit.HitRecord

import Data.List

data Object = Sphere S.Sphere

sphere :: Point -> Double -> Object
sphere c r = Sphere $ S.Sphere c r

hit :: Ray -> Object -> Double -> Double -> Maybe HitRecord
hit r (Sphere s) tMin tMax = S.hitSphere r s tMin tMax

hitList :: Ray -> [Object] -> Double -> Double -> Maybe HitRecord
hitList r objs tMin tMax =
  fst $ foldl' foldFn start objs where
    foldFn (cRec, cMax) obj =
      case hit r obj tMin cMax of
        Nothing -> (cRec, cMax)
        Just hitRec -> 
          (Just hitRec, hitT hitRec)
    start = (Nothing, tMax)