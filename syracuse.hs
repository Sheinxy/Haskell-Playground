module Syracuse where

-- Syracuse sequence for N
syracuse :: Int -> [Int]
syracuse = iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

-- Flight time for a syracuse sequence for N
syracuseFlight :: Int -> Int
syracuseFlight = length . takeWhile (/= 1) . syracuse

-- Flight time for a syracuse sequence for N
awesomeSyracuseFlight :: Int -> Int
awesomeSyracuseFlight = length . takeWhile (/= 1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)