import Data.Char

respondPalindromes contents = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome")
