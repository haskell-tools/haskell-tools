{-# LANGUAGE MagicHash,
             PolyKinds,
             MultiParamTypeClasses
             #-}

module InKind where

-- TODO: unboxed kinds

class C (f :: k# -> *) (a :: k#) (b :: *) where  {-* Magichash, Magichash *-} --PolyKinds, MultiParamTypeClasses
    g :: f a -> b
