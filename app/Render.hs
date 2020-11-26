module Render where

import Control.Parallel.Strategies

import Bmp.Bmp
import Vec3
import Ray

import Sphere

import qualified Hit.Hit as H
import Hit.HitRecord

degToRad :: Double -> Double
degToRad d = d * pi / 180

data Image = Image {
  imageAspectRatio :: Double,
  imageWidth :: Int,
  imageHeight :: Int
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

inf = 1/0 :: Double

cameraRay :: Camera -> Double -> Double -> Ray
cameraRay camera  u v = 
  let Point o = cameraOrigin camera
      llc = cameraLowerLeftCorner camera
      camH = cameraHorizontal camera
      camV = cameraVertical camera
  in Ray (cameraOrigin camera) (llc `add` (camH `timesConst` u) `add` (camV `timesConst` v) `sub` o)

rayColor :: Ray -> [H.Object] -> Color
rayColor r world =
  case H.hitList r world 0 inf of
    Just hitRec ->
      let n = hitNormal hitRec
      in vecToColor ((vec (x n + 1) (y n + 1) (z n + 1)) `timesConst` 0.5)
    Nothing ->
      let d = unit (rayDirection r)
          t = 0.5 * (y d + 1.0)
       in vecToColor $ timesConst (vec 1 1 1) (1-t) `add` timesConst (vec 0.5 0.7 1.0) t
       

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

setupImage :: Int -> Int -> Image
setupImage w h =
  Image {
    imageAspectRatio = fromIntegral w / fromIntegral h,
    imageWidth = w,
    imageHeight = h
  }

renderScene :: Int -> Int -> BmpImg
renderScene w h = 
  let image = setupImage w h
      camera = setupCamera image (point 0 0 0) 2.0
      world = [H.sphere (point 0 0 (-1)) 0.5, H.sphere (point 0 (-100.5) (-1)) 100]

      genPixel :: Int -> Int -> Pixel
      genPixel x y = 
        colorToPixel c where
          xf = fromIntegral x :: Double
          yf = fromIntegral y :: Double
          u = xf / fromIntegral (imageWidth image - 1)
          v = yf / fromIntegral (imageHeight image - 1)
          r = cameraRay camera u v
          c = rayColor r world
      
      gen :: Int -> Int -> [[Pixel]]
      gen w h = [[genPixel x y | x <- [0 .. w -1]]| y <- [0 .. h -1]] `using` (parList rdeepseq)
  in BmpImg $ map ImgRow (gen w h)