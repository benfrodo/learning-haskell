import qualified Data.List as M -- (nub, sort) would load only those two functions from the module
import qualified Data.Char as C
import qualified Data.Map as Map
import qualified Data.Set as Set  

-- no. of unique items in a list
numUniques :: (Eq a) => [a] -> Int
numUniques = length . M.nub

-- search for a sublist, e.g. search "ben" "my name is ben" = True
search :: (Eq a) => [a] -> [a] -> Bool  
search needle haystack =   
    let nlen = length needle  
    in  foldl (\acc x -> if M.take nlen x == needle then True else acc) False (M.tails haystack)  
-- Data.List.isInfixOf function does the same job

-- find function is very useful, safer than dropWhile if we may not get a return value. Learn about Maybe types


-- Caesar cypher
caesar :: Int -> String -> String 
caesar key message = 
    let nums = map C.ord message
        shifted = map (+key) nums
    in map C.chr shifted

decodeCaesar :: Int -> String -> String 
decodeCaesar key cyphertext = caesar (negate key) cyphertext

-- simple dictionary lookup using Maybe types and other stuff
lookUp :: (Eq k) => k -> [(k,v)] -> Maybe v
lookUp key [] = Nothing 
lookup key ((k,v):xs) = if key == k
                        then Just v
                        else lookUp key xs


-- or written with a fold instead of recursion
lookUp' :: (Eq k) => k -> [(k,v)] -> Maybe v
lookUp' key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing 
-- this function exists as lookup in Data.List

-- Data.Set contains things like unions, intersections and differences as in proper mathematical sets
