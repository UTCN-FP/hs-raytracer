module Sphere where

import Hit.HitRecord
import Ray
import Vec3

data Sphere = Sphere {sphereCenter :: Point, sphereRadius :: Double}

hitSphere :: Ray -> Sphere -> Double -> Double -> Maybe HitRecord
hitSphere ray sphere minT maxT =
  let oc = (pointToVec $ rayOrigin ray) `sub` (pointToVec $ sphereCenter sphere)
      rDir = rayDirection ray
      sR = sphereRadius sphere
      a = lenSqrd rDir
      halfB = (oc `dot` rDir)
      c = lenSqrd oc - sR * sR
      discriminant = halfB * halfB - a * c
   in if discriminant < 0
        then Nothing
        else
          let between x lo hi = lo <= x && x <= hi
              sqrtd = sqrt discriminant
              root1 = (- halfB - sqrtd) / a
              root2 = (- halfB + sqrtd) / a

              mkRec root =
                Just $ hitRecord root p ray outN
                where
                  p = ray `at` root
                  outN = ((pointToVec p) `sub` (pointToVec $ sphereCenter sphere)) `divideConst` (sphereRadius sphere)
           in if between root1 minT maxT
                then mkRec root1
                else
                  if between root2 minT maxT
                    then mkRec root2
                    else Nothing
