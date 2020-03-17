import           Data.List
import           Test.QuickCheck

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lower ++ x : qsort higher
  where
    lower = filter (< x) xs
    higher = filter (>= x) xs

prop_idempotent xs = qsort xs == qsort (qsort xs)

check = verboseCheck (prop_idempotent :: [Integer] -> Bool)

prop_minimum xs = head (qsort xs) == minimum xs

prop_minimum' xs = not (null xs) ==> head (qsort xs) == minimum xs

check' = verboseCheck (prop_minimum' :: [Integer] -> Property)

