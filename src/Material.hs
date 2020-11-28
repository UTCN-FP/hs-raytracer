module Material where

import Vec3

data Lambertian =  Lambertian {lambertianAlbedo :: Color} 
data Metallic = Metallic {metallicAlbedo :: Color}

data Material = Diffuse Lambertian | Metal Metallic

diffuse :: Color -> Material
diffuse albedo = Diffuse $ Lambertian albedo

metal :: Color -> Material
metal albedo = Metal $ Metallic albedo