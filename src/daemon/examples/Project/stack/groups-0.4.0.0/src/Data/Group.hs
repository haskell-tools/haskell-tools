module Data.Group where

import Data.Monoid

-- |A 'Group' is a 'Monoid' plus a function, 'invert', such that: 
--
-- @a \<> invert a == mempty@
--
-- @invert a \<> a == mempty@
class Monoid m => Group m where
  invert :: m -> m
  -- |@'pow' a n == a \<> a \<> ... \<> a @
  --
  -- @ (n lots of a) @
  --
  -- If n is negative, the result is inverted.
  pow :: Integral x => m -> x -> m
  pow x0 n0 = case compare n0 0 of
    LT -> invert . f x0 $ negate n0
    EQ -> mempty
    GT -> f x0 n0
    where
      f x n 
        | even n = f (x `mappend` x) (n `quot` 2)
        | n == 1 = x
        | otherwise = g (x `mappend` x) ((n - 1) `quot` 2) x
      g x n c
        | even n = g (x `mappend` x) (n `quot` 2) c
        | n == 1 = x `mappend` c
        | otherwise = g (x `mappend` x) ((n - 1) `quot` 2) (x `mappend` c)
  
instance Group () where
  invert () = ()
  pow () _ = ()

instance Num a => Group (Sum a) where
  invert = Sum . negate . getSum
  {-# INLINE invert #-}
  pow (Sum a) b = Sum (a * fromIntegral b)
  
instance Fractional a => Group (Product a) where
  invert = Product . recip . getProduct
  {-# INLINE invert #-}
  pow (Product a) b = Product (a ^^ b)

instance Group a => Group (Dual a) where
  invert = Dual . invert . getDual
  {-# INLINE invert #-}
  pow (Dual a) n = Dual (pow a n)

instance Group b => Group (a -> b) where
  invert f = invert . f
  pow f n e = pow (f e) n

instance (Group a, Group b) => Group (a, b) where
  invert (a, b) = (invert a, invert b)
  pow (a, b) n = (pow a n, pow b n)
  
instance (Group a, Group b, Group c) => Group (a, b, c) where
  invert (a, b, c) = (invert a, invert b, invert c)
  pow (a, b, c) n = (pow a n, pow b n, pow c n)

instance (Group a, Group b, Group c, Group d) => Group (a, b, c, d) where
  invert (a, b, c, d) = (invert a, invert b, invert c, invert d)
  pow (a, b, c, d) n = (pow a n, pow b n, pow c n, pow d n)

instance (Group a, Group b, Group c, Group d, Group e) => Group (a, b, c, d, e) where
  invert (a, b, c, d, e) = (invert a, invert b, invert c, invert d, invert e)
  pow (a, b, c, d, e) n = (pow a n, pow b n, pow c n, pow d n, pow e n)

-- |An 'Abelian' group is a 'Group' that follows the rule:
-- 
-- @a \<> b == b \<> a@
class Group g => Abelian g

instance Abelian ()

instance Num a => Abelian (Sum a)

instance Fractional a => Abelian (Product a)

instance Abelian a => Abelian (Dual a)

instance Abelian b => Abelian (a -> b)

instance (Abelian a, Abelian b) => Abelian (a, b)

instance (Abelian a, Abelian b, Abelian c) => Abelian (a, b, c)

instance (Abelian a, Abelian b, Abelian c, Abelian d) => Abelian (a, b, c, d)

instance (Abelian a, Abelian b, Abelian c, Abelian d, Abelian e) => Abelian (a, b, c, d, e)
