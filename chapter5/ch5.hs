                                                -- Recursion

                                                -- 5.2 Maximum awesome

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x                                -- if there's one value then the max is that value
maximum' (x:xs)                                 -- split list into a head (x) and tail (xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs                 -- where binding to define maxTail as the max of the rest of the list (which is the prior list minus the head)

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "maximum of empty list"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)         -- max compares two values. We feed it the head and the head of the tail list

minimum' :: (Ord a) => [a] -> a
minimum' [] = error "minimum of empty list"
minimum' [x] = x                                -- returns single value of a single valued list
minimum' (x:xs)                                 -- separates a list into a head (x) and then another list of the remaining list (tail)
    | x < minTail = x                           -- checks if head is less than the minimum of the remaining list (i.e., the tail of the list)
    | otherwise = minTail                       -- if it isn't then we separate the tail into a head and another tail and perform the same operation
    where minTail = minimum' xs                 -- xs is a smaller list than (x:xs) as it just represents the tail

minimum'' :: (Ord a) => [a] -> a
minimum'' [] = error "minimum of empty list"
minimum'' [x] = x
minimum'' (x:xs) = min x (minimum'' xs)

                                                -- 5.3 A few more recursive functions

                                                -- replicate n x times
replicate' :: (Num i, Ord i) => i -> a -> [a]   -- input can be of any Num class or Ord class
replicate' n x
    | n <= 0 = []
    | otherwise = x:replicate' (n-1) x          -- eventually n-1 reaches our edge (or base) condition
                                                -- replicate' 3 x; outputs x
                                                -- replicate' (3-1=2) x; ouputs x
                                                -- replicate' (2-1=1) x; outputs x
                                                -- replicate' (1-1=0) x; outputs []
                                                -- total output: xxx

                                                -- take a certain number of elements from a list
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _                                       -- we don't care what the list value is
    | n <= 0 = []                               -- if n <= 0 then just return an empty list
take' _ []   = []                               -- if we pass in an empty list then return an empty list
take' n (x:xs) = x : take' (n-1) xs             -- return head of list until n reaches the base/edge condition

reverse' :: [a] -> [a]
reverse' [] = []                                -- edge condition
reverse' (x:xs) = reverse' xs ++ [x]            -- split a list to head and a tail, the reversed list is equal to the reversed tail and the head at the end

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []                                  -- if any list is empty then return empty list
zip' [] _ = []                                  -- if any list is empty then return empty list
zip' (x:xs) (y:ys) = (x,y):zip' xs ys           -- take two lists (x:xs and y:ys) and return their heads while zipping their tails

elem' :: (Eq a) => a -> [a] -> Bool             -- checks if element is in list
elem' a [] = False                              -- if empty list is inserted then return false
elem' a (x:xs)                                  -- get some list (x:xs)
    | a == x = True                             -- if head is the element then return true
    | otherwise = a `elem'` xs                  -- otherwise check if elemen is an element in the tail

                                                        -- 5.4 Quick, sort!
quicksort :: (Ord a) => [a] -> [a]                      -- type signature!
quicksort [] = []                                       -- if empty list is passed in, then return empty list
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted             -- put head of list in the middle of the two lists