module Hit.HitRecord where

import Vec3
import Ray

data HitRecord = HitRecord {
  hitPoint :: Point,
  hitNormal :: Vec3,
  hitT :: Double,
  hitFrontFace :: Bool
}

hitRecord :: Double -> Point -> Ray -> Vec3 -> HitRecord
hitRecord t point ray outNormal =
  let ff = (rayDirection ray) `dot` outNormal < 0
      n = if ff then outNormal else neg outNormal
  in  HitRecord {
    hitPoint = point,
    hitT = t,
    hitFrontFace = ff,
    hitNormal = n
  } 