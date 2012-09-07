data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)  

singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
	           
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)   
	   | x == a = Node x left right  
	   | x < a  = Node a (treeInsert x left) right  
	   | x > a  = Node a left (treeInsert x right)  

treeCreate :: (Ord a) => [a] -> Tree a
treeCreate x = foldr treeInsert EmptyTree $ reverse x

treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False  
treeElem x (Node a left right)  
	 | x == a = True  
         | x < a  = treeElem x left  
         | x > a  = treeElem x right  

treeInfix :: (Ord a) => Tree a -> [a]
treeInfix EmptyTree = []
treeInfix (Node x left right) = treeInfix left ++ [x] ++ treeInfix right

treePrefix :: (Ord a) => Tree a -> [a]
treePrefix EmptyTree = []
treePrefix (Node x left right) =  [x] ++ treePrefix left ++ treePrefix right

treeHeight :: Tree a -> Int 
treeHeight EmptyTree = 0
treeHeight ( Node x EmptyTree EmptyTree) = 0
treeHeight ( Node x left right) = 1 + treeHeight left + treeHeight right 
	   
main = print(treeInfix a, " ", treeHeight $ treeCreate [4,2,1,5], " ", treeCreate [2,1,5,4] )
     where
	a = treeCreate [5,7,2,4,1,10,3,8]
