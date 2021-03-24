doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                    then x
                    else doubleMe x


boomBang xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

--length' xs = sum[1 | _ <- xs]

factorial :: Integer -> Integer 
factorial x = product [1..x]

-- learning pattern matching

head' :: [a] -> a
head' [] = error "You can't call head on an empty list"
head' (x:_) = x

factorial' :: (Integral a) => a -> a
factorial' 0 = 1
factorial' x = x * factorial' (x-1)

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- 'as patterns'
capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]  

-- guards and where bindings
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"
    where bmi = weight / height^2

max' :: (Ord a) => a -> a -> a
max' x y
    | x > y = x
    | otherwise = y


calcBMI :: (RealFloat a) => [(a,a)] -> [a]
calcBMI xs = [ bmi w h | (w,h) <- xs]
    where bmi weight height = weight / height^2

-- let bindings
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  

-- recursion
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys


elem' :: (Eq a) => a -> [a] -> Bool  
elem' a [] = False  
elem' a (x:xs)  
    | a == x    = True  
    | otherwise = a `elem'` xs 

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  


length' :: (Num a) => [a] -> a
length' [] = 0
length' (x:xs) = 1 + length' xs
