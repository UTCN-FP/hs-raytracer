module Sphere where

import Vec3
import Ray

data Sphere = Sphere {sphereCenter :: Point, sphereRadius :: Double}

hitSphere :: Ray -> Sphere -> Double
hitSphere ray sphere =
  let oc = (pointToVec $ rayOrigin ray) `sub` (pointToVec $ sphereCenter sphere)
      rDir = rayDirection ray
      sR = sphereRadius sphere
      a = rDir `dot` rDir
      b = 2 * (oc `dot` rDir)
      c = (oc `dot` oc) - sR * sR
      discriminant = b*b - 4*a*c
  in if discriminant < 0 then -1 else ((- b - sqrt discriminant ) / (2 * a))

