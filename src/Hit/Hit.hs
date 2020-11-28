module Hit.Hit where

import qualified Sphere as S
import Ray

import Vec3

import Material
import Hit.HitRecord

import Util.Random

import Data.List

data Object = Sphere S.Sphere

sphere :: Point -> Double -> Material -> Object
sphere c r m = Sphere $ S.Sphere c r m

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

scatter :: Material -> Ray -> HitRecord -> Rnd Random (Maybe (Color, Ray))
scatter (Diffuse mat) _ hitRec = do 
  v <- randomVec
  let p = hitPoint hitRec
      n = hitNormal hitRec
      scatterDir = n `add` v
      d = if nearZero scatterDir then n else scatterDir
  return $ Just (lambertianAlbedo mat, Ray p d)
scatter (Metal mat) ray hitRec = do
  let refl = reflect (unit (rayDirection ray)) (hitNormal hitRec)
      scattered = Ray (hitPoint hitRec) refl
  if ((rayDirection scattered) `dot` (hitNormal hitRec)) > 0 then 
    return $ Just (metallicAlbedo mat, scattered)
  else 
    return Nothing