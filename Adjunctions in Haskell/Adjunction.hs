{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Adjunction where

import Control.Category
import Control.Category.Unicode
import Prelude (undefined, flip)

class Functor f where
  fmap ∷ (a → b) → f a → f b

class (Functor f, Functor g) ⇒ Adjunction f g where
  ε ∷ f (g a) → a               -- counit (natural transformation)
  η ∷ a → g (f a)               -- unit (natural transformation)
  φ ∷ (f a → b) → a → g b       -- two natural isomorphisms
  ψ ∷ (b → g a) → f b → a

  ε   = ψ id
  η   = φ id
  φ f = fmap f ∘ η
  ψ g = ε ∘ fmap g

class Adjunction f g ⇒ Monad f g where
  μ ∷ g (f (g (f a))) → g (f a)
  μ = fmap ε

  (>>=) ∷ g (f a) → (a → g (f b)) → g (f b)
  x >>= f = μ (fmap (fmap f) x)

class Adjunction f g ⇒ Comonad f g where
  δ ∷ f (g a) → f (g (f (g a)))
  δ = fmap η

  (=>>) ∷ f (g a) → (f (g a) → b) → f (g b)
  x =>> f = fmap (fmap f) (δ x)

type Prod d = (,) d

instance Functor (Prod d) where
  fmap f (x,y) = (x, f y)

type Hom e = (→) e

instance Functor (Hom e) where
  fmap = (∘)

instance d ~ e ⇒ Adjunction (Prod d) (Hom e) where
  -- ε ∷ f (g a) → a
  -- ε ∷ f (e → a) → a
  -- ε ∷ (e, e → a) → a
  -- ε ∷ (e, e → a) → a
  ε (y,f) = f y

  -- η ∷ a → g (f a)
  -- η ∷ a → (e → f a)
  -- η ∷ a → e → f a
  -- η ∷ a → e → (e,a)
  η x y = (y,x)

  -- φ ∷ (f a → b) → a → g b
  -- φ f     = fmap f ∘ η
  -- φ f x y = (fmap f ∘ η) x y
  -- φ f x y = (fmap f ∘ \x y → (y,x)) x y
  -- φ f x y = (fmap f ∘ \x → \y → (y,x)) x y
  -- φ f x y = (fmap f (\y → (y,x))) y
  -- φ f x y = (f ∘ (\y → (y,x))) y
  φ f x y = f (y,x)

  -- ψ ∷ (b → g a) → f b → a
  -- ψ g       = ε ∘ fmap g
  -- ψ g (y,f) = (ε ∘ fmap g) (y,f)
  -- ψ g (y,f) = ε (fmap g (y,f))
  -- ψ g (y,f) = ε (y,g f)
  -- ψ g (y,f) = (g f) y
  ψ g (y,f) = g f y

instance d ~ e ⇒ Monad   (Prod d) (Hom e) -- ≅ "State"
instance d ~ e ⇒ Comonad (Prod d) (Hom e) -- ≅ "Store"

-- Adjunction.hs ends here
