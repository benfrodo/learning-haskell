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
    