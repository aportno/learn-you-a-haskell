-- ch7 Modules

-- 7.1 loading modules

import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

-- 7.6 making your own modules
