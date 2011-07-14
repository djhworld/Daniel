module Daniel.Utils.List
(
    groupSortAndRankItems,
    split
)
where
import Data.List (group, sort)

groupSortAndRankItems :: (Ord a) => [a] -> [(Int, a)]
groupSortAndRankItems xs = reverse $ sort $ map (\x -> ((length x), head x)) $ group $ sort xs

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split x ys = takeWhile (/=x) ys : split x (drop 1 (dropWhile (/=x) ys))
