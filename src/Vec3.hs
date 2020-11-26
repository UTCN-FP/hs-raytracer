module Vec3 where

import Bmp.Bmp

data Vec3 = Vec3 {x:: Double, y:: Double, z:: Double}
newtype Color = Color Vec3
newtype Point = Point Vec3

add v1 v2 = Vec3 (x v1 + x v2) (y v1 + y v2) (z v1 + z v2)

neg v = Vec3 (- x v) (- y v) (- z v)

sub v1 v2 = add v1 (neg v2)

timesConst v1 t = Vec3 (x v1 * t) (y v1 * t) (z v1 * t)

times v1 v2 = Vec3 (x v1 * x v2) (y v1 * y v2) (z v1 * z v2)

divideConst v1 t = timesConst v1 (1 / t)

lenSqrd v1 = (x v1) ^ 2 + (y v1) ^ 2 + (z v1) ^ 2

len v1 = sqrt $ lenSqrd v1

get v1 0 = x v1
get v1 1 = y v1
get v1 2 = z v1

dot v1 v2 = x v1 * x v2 + y v1 * y v2 + z v1 * z v2

cross v1 v2 = Vec3 vx vy vz where
  vx = y v1 * z v2 - z v1 * y v2
  vy = z v1 * x v2 - x v1 * z v2
  vz = x v1 * y v2 - y v1 * x v2

unit v1 = v1 `divideConst` (len v1)

vecToColor v1 = Color $ Vec3 (255 * x v1) (255 * y v1) (255 * z v1)

colorToPixel (Color c) = Pixel (round (x c)) (round (y c)) (round (z c))

vec x y z = Vec3 x y z
color r g b = vecToColor $ vec r g b
point x y z = Point $ vec x y z

pointToVec (Point p) = p
vecToPoint v = Point v