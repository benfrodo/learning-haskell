-- higher order functions

-- partial application
compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred = compare 100 

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys  

-- quick sort using filter
quicksort :: (Ord a) => [a] -> [a]    
quicksort [] = []    
quicksort (x:xs) =     
    let smallerSorted = quicksort (filter (<=x) xs)  
        biggerSorted = quicksort (filter (>x) xs)   
    in  smallerSorted ++ [x] ++ biggerSorted  

-- largest number under 100,000 that's divisible by 3829
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999..])
    where p x = x `mod` 3829 == 0

-- sum of all odd squares < 10,000
sumSquares :: (Integral a) => a
sumSquares =  sum (takeWhile (<10^4) (filter odd (map (^2) [1 ..])))


-- for all starts in [1..100], how many Collatz sequences have chain > 15
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n: chain(n `div` 2)
    | otherwise = n: chain(3*n + 1)

numLongChains :: Int 
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15
    

-- lambdas, anonymous functions
-- Collatz again but using a lambda instead of a where clause
numberLongChains :: Int  
numberLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))  

-- sum using folds and lambdas
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- or even simpler, although less clear
sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

-- map with folds
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x:acc) [] xs

-- simpler using foldl1
sum''' :: (Num a) => [a] -> a
sum''' = foldl1 (+)

-- how many terms for the sum of sqrt's of natural numbers to be >1000
sqrtSums :: Int 
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- FUNCTION APPLICATION W $
-- map ($ 3) [(4+), (10*), (^2), sqrt] 

-- Function composition
fn x = ceiling (negate (tan (cos (max 50 x))))

fn' = ceiling . negate . tan . cos . max 50

-- can rewrite the above problem of summing odd squares under 1000
oddSquareSum' :: Integer 
oddSquareSum' = sum . takeWhile (<10^4) . filter odd . map (^2) $ [1..]

-- or a user-friendly way
oddSquareSum'' :: Integer  
oddSquareSum'' =   
    let oddSquares = filter odd $ map (^2) [1..]  
        belowLimit = takeWhile (<10000) oddSquares  
    in  sum belowLimit  
