import Data.Char (isAlpha, isDigit,toLower)

--STEP  #1  pattern matching
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc [] = acc              
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs   

--STEP  #2  Reverse String 
revString :: String -> String
revString = myFoldl (flip (:)) []

--STEP  #3  makes all characters lowercase
mapToLower :: String -> String
mapToLower = map toLower 

-- returns true if te character is either a letter or a digit
notPunct :: Char -> Bool
notPunct ch = isAlpha ch || isDigit ch

--STEP  #4 removes all characters that are neither letters nor digits from a String
remove :: String -> String
remove = filter notPunct



-- 
disregard :: String -> String
disregard = mapToLower . remove


-- 
isPalindrome :: String -> Bool
--isPalindrome str = str == reverse str
isPalindrome str = str == revString str

-- 
simplePalin :: String -> Bool
simplePalin str = isPalindrome (disregard  str)

main :: IO ()
main = do
  let inputString = "Live on, Time, emit no evil."
  putStrLn $ "Input String: " ++ inputString
  putStrLn $ "Is it a palindrome? " ++ show (simplePalin inputString)
