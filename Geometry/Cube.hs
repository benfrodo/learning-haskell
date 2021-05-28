module Geometry.Cube
(
    volume,
    area
) where

import qualified Geometry.Cuboid as Cuboid

volume :: Float -> Float 
volume length = Cuboid.volume length length length

area :: Float -> Float
area length = Cuboid.area length length length
