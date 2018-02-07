{-# LANGUAGE GADTs, TypeFamilies #-}

module GDataInstDecl where

data family G a b                      {-* TypeFamilies *-}
data instance G [a] b where            
   GExistential :: c -> G [a] b        {-* GADTSyntax, GADTs + ExistentialQuantification *-}
   GSpecResTy   :: G [Int] b           {-* GADTSyntax, GADTs + ExistentialQuantification *-}
   GContext     :: Eq a => G [a] b     {-* GADTSyntax, GADTs + ExistentialQuantification *-}
   GRegular     :: G [a] b             {-* GADTSyntax, TypeFamilies *-}
