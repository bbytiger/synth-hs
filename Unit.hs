import Test.QuickCheck

-- for string DSL

prop_reverseReverse :: [Int] -> Bool
prop_reverseReverse xs = reverse (reverse xs) == xs

-- for scheme subset

