-- Typed stack 

data Stack' v = Stack' [v] Int deriving (Show)

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
     print("Hello World", push (Stack [1,2,3] 4) 5, pop (Stack [1,2,3] 4), top $ pop $ push (Stack [1,2,3] 4) 5)
     print(Stack' [1,2,3] 4, push' (push' (Stack' ["Manoj","Eknath","Abhay"] 4) "Sonali") "Lakshmi")

