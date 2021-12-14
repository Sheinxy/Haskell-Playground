module Polynomial (Polynomial (Polynomial), coef, order, prod, add) where

import Data.Ord
import Data.List
import Data.Map (fromList, findWithDefault)

data Polynomial a = Polynomial [(Int, a)]

coef :: Num a => Int -> Polynomial a -> a
coef n (Polynomial c) = findWithDefault (fromInteger 0) n (fromList c)

order :: Polynomial a -> Int
order (Polynomial c) = maximum . map fst $ c

prod :: (Eq a, Num a) => Polynomial a -> Polynomial a -> Polynomial a
prod p1 p2 = Polynomial coefs
  where coefs = filter ((/= 0) . snd) [(k, cauchy k) | k <- [0 .. (order p1 + order p2)]]
        cauchy n = sum [coef k p1 * coef (n - k) p2 | k <- [0 .. n]]

add :: (Eq a, Num a) => Polynomial a -> Polynomial a -> Polynomial a
add p1 p2 = Polynomial coefs
  where coefs = filter ((/= (fromInteger 0)) . snd) [(k, coef k p1 + coef k p2) | k <- [0 .. max (order p1) (order p2)]]

instance Eq a => Eq (Polynomial a) where
  (==) (Polynomial c1) (Polynomial c2) = fromList c1 == fromList c2

instance Show a => Show (Polynomial a) where
  show (Polynomial []) = ""
  show (Polynomial coefs) = drop 3 . foldl (\x (k, c) -> x ++ " + " ++ (show c) ++ "*x^" ++ (show k)) "" . 
                            sortBy (comparing fst) $ coefs
  showsPrec _ x = (++) (show x)
