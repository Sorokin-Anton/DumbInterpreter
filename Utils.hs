module Utils ( splitWhen, splitAtList, splitOn ) where

splitWhen :: (a -> Bool) -> [ a ] -> [ [ a ] ]
splitWhen cond xs = case break cond xs of
    ( s, [] ) -> [ s ]
    ( s1, s2 ) -> s1 : splitWhen cond (tail s2)

splitOn :: Eq a => a -> [ a ] -> [ [ a ] ]
splitOn x = splitWhen (== x)

splitAtList :: [ Int ] -> [ a ] -> [ [ a ] ]
splitAtList nums xs = map (map fst) $ splitWhen (\( x, n ) -> n `elem` nums)
    (zip xs [ 0 .. ])
