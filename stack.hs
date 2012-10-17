-- Typed stack 

data Stack' v = Stack' [v] Int 
     | Stack2
     | Stack3 v 
     deriving (Show)

Stack2 = Stack' [] 0 
Stack3 v = Stack' (v:[]) 1

data Month = Jan | Feb | Mar | Apr | May deriving (Ord,Eq,Show) 
 
push' :: (Ord v) => Stack' v -> v -> Stack' v
push' (Stack' l m) a = if m <= length l then Stack' l m else Stack' (l ++ [a]) m

--simple integer stack
data Stack = Stack [Int] Int deriving (Show) 
 
push :: Stack -> Int -> Stack
push (Stack l m) a = if m < length l then Stack l m else Stack (l ++ [a]) m

pop :: Stack -> Stack 
pop (Stack l m) = if length l == 0 then Stack l m else Stack (init l) m

top :: Stack -> Maybe Int 
top (Stack l m) = if length l == 0 then Nothing else Just $ last l

main = do
       let
	st = Stack' [] 5
	st1 = Stack' [] 5
       let
        st2 = push' st1 Jan	
       print(push' st "Eknath", st2)
       print("Hello World", push (Stack [1,2,3] 4) 5, pop (Stack [1,2,3] 4), top $ pop $ push (Stack [1,2,3] 4) 5, push' st2 Feb)
       print(Stack' [1,2,3] 4, push' (push' (Stack' ["Manoj","Eknath","Abhay"] 4) "Sonali") "Lakshmi", Stack3 5)

