module Decl.ClassInfix where

import Control.Applicative

class Applicative f => MonoidApplicative f where
   infixl 4 +<*>
   (+<*>) :: f (a -> a) -> f a -> f a

   infixl 5 ><
   (><) :: Monoid a => f a -> f a -> f a
