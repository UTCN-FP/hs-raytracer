module Hit.HitRecord where

import Vec3
import Ray

import Material

data HitRecord = HitRecord {
  hitPoint :: Point,
  hitNormal :: Vec3,
  hitT :: Double,
  hitFrontFace :: Bool,
  hitMaterial :: Material
}

hitRecord :: Double -> Point -> Ray -> Vec3 -> Material -> HitRecord
hitRecord t point ray outNormal mat =
  let ff = (rayDirection ray) `dot` outNormal < 0
      n = if ff then outNormal else neg outNormal
  in  HitRecord {
    hitPoint = point,
    hitT = t,
    hitFrontFace = ff,
    hitNormal = n,
    hitMaterial = mat
  } 