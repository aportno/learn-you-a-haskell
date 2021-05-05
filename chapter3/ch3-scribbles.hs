removeNonUppercase :: [Char] -> [Char]
removeNonUppercase str = [c | c <- str, c `elem` ['A'..'Z']]