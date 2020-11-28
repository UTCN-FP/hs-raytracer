module Render where

import Control.Parallel.Strategies

import Bmp.Bmp
import Vec3
import Ray

import Hit.Hit
import Hit.HitRecord
import Util.Util
import Util.Random
import Material

import Data.List

degToRad :: Double -> Double
degToRad d = d * pi / 180

data Image = Image {
  imageAspectRatio :: Double,
  imageWidth :: Int,
  imageHeight :: Int,
  imageNrSamples :: Int,
  imageMaxDepth :: Int
}

data Camera = Camera {
  cameraViewportHeight :: Double,
  cameraViewportWidth :: Double,
  cameraFocalLength :: Double,
  cameraOrigin :: Point,
  cameraHorizontal :: Vec3,
  cameraVertical :: Vec3,
  cameraLowerLeftCorner :: Vec3
}

data Scene = Scene {
  sceneImage :: Image,
  sceneCamera :: Camera
}

cameraRay :: Camera -> Double -> Double -> Ray
cameraRay camera  u v = 
  let Point o = cameraOrigin camera
      llc = cameraLowerLeftCorner camera
      camH = cameraHorizontal camera
      camV = cameraVertical camera
  in Ray (cameraOrigin camera) (llc `add` (camH `timesConst` u) `add` (camV `timesConst` v) `sub` o)

rayColor :: Ray -> [Object] -> Int -> Random -> (Vec3, Random)
rayColor _ _ 0 seed = (vec 0 0 0, seed)
rayColor r world depth seed =
  case hitList r world 0.001 inf of
    Just hitRec ->
      let mat = hitMaterial hitRec
      in case scatter mat r hitRec seed of 
        Nothing -> (vec 0 0 0, seed)
        Just (att, sc, seed') -> 
          (colorToVec att `times` rc, seed'') where (rc, seed'') = rayColor sc world (depth - 1) seed'
    Nothing ->
      let d = unit (rayDirection r)
          t = 0.5 * (y d + 1.0)
          interP = timesConst (vec 1 1 1) (1-t) `add` timesConst (vec 0.5 0.7 1.0) t
       in (interP, seed)
       

setupCamera :: Image -> Point -> Double -> Camera
setupCamera image (Point orig) vpHeight =
  let fl = 1 :: Double
      vpWidth = (imageAspectRatio image) * vpHeight
      h = vec vpWidth 0 0
      v = vec 0 vpHeight 0
      llc = orig `sub` (divideConst h 2) `sub` (divideConst v 2) `sub` (vec 0 0 fl)
  in Camera {
      cameraOrigin = Point orig,
      cameraViewportHeight = vpHeight,
      cameraViewportWidth = vpWidth,
      cameraFocalLength = fl,
      cameraHorizontal = h,
      cameraVertical = v,
      cameraLowerLeftCorner = llc
    }

setupImage :: Int -> Int -> Int -> Int -> Image
setupImage w h nrSamples maxDepth =
  Image {
    imageAspectRatio = fromIntegral w / fromIntegral h,
    imageWidth = w,
    imageHeight = h,
    imageNrSamples = nrSamples,
    imageMaxDepth = maxDepth
  }

renderScene :: Int -> Int -> Random -> Int -> BmpImg
renderScene w h seed nrSamples = 
  let image = setupImage w h nrSamples 50
      camera = setupCamera image (point 0 0 0.5) 2.0
      world = [sphere (point 0 0 (-1)) 0.5 (diffuse $ color 1 0 0 nrSamples), 
               sphere (point (-1.3) 0 (-1)) 0.5 (metal $ color 0 1 0 nrSamples),
               sphere (point 1.3 0 (-1)) 0.5 (metal $ color 0 0 1 nrSamples),
               sphere (point 0 (-100.5) (-1)) 100 (diffuse $ color 1 1 1 nrSamples)
              ]

      genPixel :: Int -> Int -> Random -> Pixel
      genPixel x y seed =         
        colorToPixel . (`vecToColor` nrSamples) $ foldl add (vec 0 0 0) $ unfoldr fn (seed, nrSamples) where
          xf = fromIntegral x :: Double
          yf = fromIntegral y :: Double
          fn (_, 0) = Nothing
          fn (rnd, smp) =
            let (r1, rnd') = nextDouble rnd
                (r2, rnd'') = nextDouble rnd'
                u = (xf + r1) / fromIntegral (imageWidth image - 1)
                v = (yf + r2) / fromIntegral (imageHeight image - 1)
                r = cameraRay camera u v
                (c, rnd''') = rayColor r world (imageMaxDepth image) rnd''
            in Just (c, (rnd''', smp - 1))
      
      gen :: Int -> Int -> [[Pixel]]
      gen w h = [[genPixel x y seed | x <- [0 .. w -1]]| y <- [0 .. h -1]] `using` (parList rdeepseq)
  in BmpImg $ map ImgRow (gen w h)