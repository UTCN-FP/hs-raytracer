module Ray where

import Vec3

data Ray = Ray {rayOrigin:: Point, rayDirection:: Vec3}

at r t = Point v where
  Point o = rayOrigin r
  v = o `add` timesConst (rayDirection r) t