import Data.Char

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

instance Functor Tree where
	 fmap f EmptyTree = EmptyTree
	 fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

data Stack' v = Stack' [v] Int deriving (Show)

push' :: (Ord v) => Stack' v -> v -> Stack' v
push' (Stack' l m) a = if m <= length l then Stack' l m else Stack' (a:l) m

instance Functor Stack' where
	fmap f (Stack' v l) = Stack' (map f v) (l)

main = print(fmap (*2) $ treeCreate [3,2,4,1,5], "   ", fmap (toLower.head) $ fmap (\x -> x ++ " " ++ x) $ treeCreate ["Manoj","Lakshmi","Eknath","Sonali","Yogesh"], " ", fmap ((*10).(+2)) $ Stack' [4,3,5,6] 5)