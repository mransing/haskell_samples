import Data.List
import Data.Char
import Debug.Trace

constval = 1000000007 

--Given list of string will return the frequency of characters and removes 1 from the frequency list e.g. "AAAbbz" will return [3,2]

getfreq :: (Eq a) => [a] -> [Int]
getfreq xs = 
	let uniq = nub xs
	in reverse.sort $ filter (>1) $ map (\x -> length $ elemIndices x xs) uniq

--combination function

ncr :: (Integral a) => a -> a -> a
ncr x 0 = 1
ncr x 1 = x
ncr x y = if (x == y) then 1 
      	       else 
      	       	  let z = if ( y < (x - y)) then y else (x - y) 
      	      	  in ((foldl (*) 1 [z+1..x]) `div` (foldl (*) 1 [1..x-z])) `mod` 1000000007

somefunc :: (Eq a) => [a] -> Int
somefunc xs = let p = getfreq xs 
	          newnum = fst $ foldl (\acc x -> (((ncr (snd acc) x) * (fst acc)) `mod` 1000000007 , (snd acc) - x )) (1, length xs) p
		  remainingelems = ((length xs) - (foldl (+) 0 p))
		  in traceShow (newnum,p,remainingelems) $ if (remainingelems <=0 ) then newnum else let a = foldl (*) 1 [1..remainingelems]
		     in traceShow a $ newnum * a
 
main = do print (getfreq [1,4,3,3,2,1,5,3,2], " ", somefunc "AAbbaazz", somefunc "AAAAb")
