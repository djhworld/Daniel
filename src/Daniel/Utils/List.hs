module Daniel.Utils.List
(
    groupSortAndRankItems
)
where
import Data.List (group, sort)

groupSortAndRankItems :: (Ord a) => [a] -> [(Int, a)]
groupSortAndRankItems xs = reverse $ sort $ map (\x -> ((length x), head x)) $ group $ sort xs
