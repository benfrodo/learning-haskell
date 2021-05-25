module Geometry  
( sphereVolume  
, sphereArea  
, cubeVolume  
, cubeArea  
, cuboidArea  
, cuboidVolume  
) where  

sphereVolume :: Float -> Float 
sphereVolume radius = (4.0/3.0) * pi * radius^3

sphereArea :: Float -> Float 
sphereArea radius = 4.0 * pi * radius^2

cubeVolume :: Float -> Float 
cubeVolume length = cuboidVolume length length length

cubeArea :: Float -> Float 
cubeArea length = cuboidArea length length length

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume a b c = a * b * c

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c = 2 * (a*b + b*c + a*c)
