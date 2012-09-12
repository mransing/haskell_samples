-- main = do   
--     line <- getLine  
--     if null line  
--         then return ()  
--         else do  
--             putStrLn $ reverseWords line  
--             main  
  
-- reverseWords :: String -> String  
-- reverseWords = unwords . map reverse . words 

main = interact palin

ispalindromes :: String -> String 
ispalindromes xs = if (xs == reverse xs) then "Palindrome" else "Not a palindrome"

palin :: String -> String
palin = unlines . map (ispalindromes) . lines

