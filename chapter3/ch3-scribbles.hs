removeNonUppercase :: [Char] -> [Char] {- synonymous with String -> String -}
removeNonUppercase str = [c | c <- str, c `elem` ['A'..'Z']]

addThree :: Integer -> Integer -> Integer -> Integer
addThree x y z = x + y + z

