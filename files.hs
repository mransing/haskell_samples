import System.IO

-- main = do 
--      contents <-readFile "iosys.txt"
--      writeFile "iosysout.txt" (palin contents)

ispalindromes :: String -> String 
ispalindromes xs = if (xs == reverse xs) then "Palindrome" else "Not a palindrome"

palin :: String -> String
palin = unlines . map (\x -> x ++ ":-" ++ ispalindromes x) . lines

main = do line <- fmap palin getContents  
          putStrLn $ line  
         