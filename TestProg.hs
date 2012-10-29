import Data.List
import Data.Char
import Debug.Trace

--Given a list , creates another list with the number of occurrence of each element

getfreq :: (Eq a) => [a] -> [(a,Int)]
getfreq xs = 
	let uniq = nub xs
	in zip uniq (map (\x -> length $ elemIndices x xs) uniq)

reversepair :: (a,b) -> (b,a)
reversepair (a,b) = (b,a)

mapreversepair :: [(a,b)] -> [(b,a)]
mapreversepair = map reversepair 

--Given a list with a number and its frequency sorts the list according to descending order of frequency e.g. [(1,4),(2,1),(3,5)] -> [(3,5), (1,4), (2,1)]

dofreqsort :: (Ord b, Ord a) => [(a,b)] -> [(a,b)]
dofreqsort xs = reverse $ mapreversepair $ sort $ mapreversepair xs

--string to number 
strtonum :: String -> Int
strtonum s = foldl (\acc x -> acc * 10 + ord x - ord '0') 0 s 

--inverse a number

inversenumber :: Int -> Int 
inversenumber x = snd $ foldl (\acc y -> (fst acc `div` 10, if fst acc == 0 then snd acc else snd acc * 10 +  fst acc `mod` 10) ) (x,0) [1..20] 

--number to string using recursion
 
numtostr :: Int -> String
numtostr 0 = []
numtostr s = chr (s `mod` 10 + ord '0') : numtostr (s `div` 10)

--non recursive number to string 

numtostr' :: Int  -> String
numtostr' x = snd $ foldl (\acc y -> if fst acc == 0 then acc else ( fst acc `div` 10 , chr ( (fst acc `mod` 10) +  ord '0' ):snd acc ) ) (x,[]) [1..20]

--quicksort of any type list 

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
	  let 
	      smallerSorted = quicksort [a | a <- xs, a <= x]
	      biggerSorted = quicksort [a | a <- xs, a > x]
	  in  
	      smallerSorted ++ [x] ++ biggerSorted


--is a number divisible by other 
isdiv :: (Integral a) =>  a -> a -> Bool
isdiv a b = p == 0 
      where p = a `mod` b

--is a number divides any number in list

isdivisible :: (Integral a) => a -> [a] -> Bool
isdivisible a xs = if any (isdiv a) xs then True else False;

--is any number in the list is divisible by given number

isdivisible' :: (Integral a) => a -> [a] -> Bool
isdivisible' a xs = if any (\x -> (a `mod` x) == 0) xs then True else False

--prime number generating between 1 and 
	      
test :: Integral a => [a] -> [a] 
test xs = foldl (\acc x -> if any (\a -> (x `mod` a) == 0) acc then acc else acc ++ [x]) [2] xs  

--prime number generating between 1 and non-recursive

test' :: Integral a => a -> [a]
test' 2 = [2]
test' 3 = [2,3]
test' a = foldl (\acc x -> if any (\a -> (x `mod` a) == 0) acc then acc else acc ++ [x]) [2] [3,4..a]

 --prime number generating between 1 and recursive

allprimes :: Integral a => [a] -> [a]
allprimes [] = [2]
allprimes (x:xs) = 
	  let y = allprimes xs
	  in if any (\a -> (x `mod` a) == 0) y then y else [x] ++ y  

--Finding quilibrium index 

equil :: Integral a => [a] -> Maybe Int
equil xs = 
      let tsum = foldr1 (+) xs
      	  intsum = scanr1 (+) xs
      	  intind = foldr (\acc x -> if tsum == 2 * x then x else acc) 0 intsum
      in elemIndex intind intsum

--Generate the list [1,3,6,10,15,21,..]

testso :: Integral a => a -> [a]
testso n = reverse $ foldl (\acc x -> head acc + x:acc) [0] [1,2..n]  

main = do 
     print (test' 100, " " , test' 2, " ", test' 3, " ", test' 4, " ", equil [6,1,4,2], " ", testso 10, strtonum "5647", numtostr' 12350, inversenumber 54321034, " ")
     print ( dofreqsort $ getfreq [3,4,6,4,1,2,7,9,3,4,5,6], " ", dofreqsort $ getfreq "my name is something i dont understand")
