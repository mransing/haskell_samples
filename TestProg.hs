import Data.List 
import Debug.Trace

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
	  let 
	      smallerSorted = quicksort [a | a <- xs, a <= x]
	      biggerSorted = quicksort [a | a <- xs, a > x]
	  in  
	      smallerSorted ++ [x] ++ biggerSorted

isdiv :: (Integral a) =>  a -> a -> Bool
isdiv a b = p == 0 
      where p = a `mod` b

isdivisible :: (Integral a) => a -> [a] -> Bool
isdivisible a xs = if any (isdiv a) xs then True else False;

isdivisible' :: (Integral a) => a -> [a] -> Bool
isdivisible' a xs = if any (\x -> (a `mod` x) == 0) xs then True else False
	      
test :: Integral a => [a] -> [a] 
test xs = foldl (\acc x -> if any (\a -> (x `mod` a) == 0) acc then acc else acc ++ [x]) [2] xs  

test' :: Integral a => a -> [a]
test' 2 = [2]
test' 3 = [2,3]
test' a = foldl (\acc x -> if any (\a -> (x `mod` a) == 0) acc then acc else acc ++ [x]) [2] [3,4..a]
 
allprimes :: Integral a => [a] -> [a]
allprimes [] = [2]
allprimes (x:xs) = 
	  let y = allprimes xs
	  in if any (\a -> (x `mod` a) == 0) y then y else [x] ++ y  

equil :: Integral a => [a] -> Maybe Int
equil xs = 
      let tsum = foldr1 (+) xs
      	  intsum = scanr1 (+) xs
      	  intind = foldr (\acc x -> if tsum == 2 * x then x else acc) 0 intsum
      in elemIndex intind intsum	  
main = print(test' 100, " " , test' 2, " ", test' 3, " ", test' 4, " ", equil [6,1,4,2])

