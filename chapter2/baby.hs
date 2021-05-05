doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = 
     if x > 100
     then x
     else x*2

test x y = x + y

lostNumbers = [1,2,3,4]

name = "Steve Buscemi"

listOfList = [[1,2,3],[4,5,6],[7,8,9]]

texasRange = [1..20]

texasRangeStep = [3,6..20]

multiples x = take x [13,26..1000]

removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]
