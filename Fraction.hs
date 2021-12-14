module Fraction where

data Fraction = Fraction Int Int

fraction :: Int -> Int -> Fraction
fraction _ 0 = error "Division by 0"
fraction n d = Fraction n' d'
  where cd = gcd n d
        n' = n `div` cd
        d' = d `div`cd

sign :: Fraction -> Int
sign (Fraction n d) = signum n * signum d

instance Num Fraction where
  (+) (Fraction n1 d1) (Fraction n2 d2) = fraction (n1 * d2 + n2 * d1) (d1 * d2)
  (*) (Fraction n1 d1) (Fraction n2 d2) = fraction (n1 * n2) (d1 * d2)
  abs (Fraction n d) = Fraction (abs n) (abs d)
  signum (Fraction n d) = Fraction (signum n * signum d) 1
  fromInteger n = Fraction (fromInteger n::Int) 1
  negate (Fraction n d) = Fraction (-n) d

instance Eq Fraction where
  (==) (Fraction n1 d1) (Fraction n2 d2) = n1' == n2' && d1' == d2'
    where (Fraction n1' d1') = fraction n1 d1
          (Fraction n2' d2') = fraction n2 d2

instance Show Fraction where
  show (Fraction n d) = show n ++ "/" ++ show d
  showsPrec _ x = (++) (show x)

instance Ord Fraction where
  (<=) f1 f2
    | sign f1 < sign f2 = True
    | sign f2 < sign f1 = False
    | sign f1 == -1 = abs f2 <= abs f1
    | otherwise = let (Fraction n1 d1, Fraction n2 d2) = (f1, f2) in n1 * d2 <= d1 * n2
