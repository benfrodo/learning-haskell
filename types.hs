data Point = Point Float Float deriving (Show)  
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
-- circle has (coords of centre), radius. rectangle has (coords of top left), (coords of bottom right)

-- Shape is datatype, so this is what we must declare
surface :: Shape -> Float  
surface (Circle _ r) = pi * r^2  
surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) *  abs (y2 - y1)  

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b)) 

baseCircle :: Float -> Shape
baseCircle  r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

-- introducing record syntax
data Person = Person { firstname :: String,
                        lastname :: String,
                        age :: Int,
                        height :: Float,
                        phoneNumber :: String,
                        flavour :: String
} deriving (Show)


-- parametrised types
data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n
