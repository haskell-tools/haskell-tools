module Decl.MinimalPragma where

class A x where
  f, g :: x -> ()
  {-# MINIMAL f,g #-}

class B x where
  fb, gb :: x -> ()
  fb = gb
  gb = fb
  {-# MINIMAL fb | gb #-}

class C x where
  fc, gc, hc :: x -> x
  gc = fc
  hc = fc
  fc = gc . hc
  {-# MINIMAL fc | (gc, hc) #-}

class D x where
  fd :: x -> x
  fd = id
  {-# MINIMAL #-}