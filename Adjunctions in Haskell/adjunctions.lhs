{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Control.Category.Unicode
import Prelude hiding (Monad)

class (Functor f, Functor g) ⇒ Adjunction f g where
  ε  ∷ f (g a) → a            -- counit (natural transformation)
  η0 ∷ a → g (f a)            -- unit (natural transformation)
  φl ∷ (f a → b) → a → g b    -- two natural isomorphisms
  φr ∷ (b → g a) → f b → a

  ε    = φr id
  η0   = φl id
  φl f = fmap f ∘ η0
  φr g = ε ∘ fmap g

class Adjunction f g ⇒ Monad f g where
  η :: a → g (f a)
  μ :: g (f (g (f a))) → g (f a)

  η = η0
  μ = fmap ε

class Adjunction f g ⇒ Comonad f g where
  coη :: f (g a) → a
  coμ :: f (g a) → f (g (f (g a)))

  coη = ε
  coμ = fmap η0

type Prod e = (,) e

-- instance Functor (Prod e) where
--   fmap f (x,y) = (x, f y)

type Hom e = (→) e

-- instance Functor (Hom e) where
--   fmap = (∘)

instance d ~ e ⇒ Adjunction (Prod d) (Hom e) where
  ε (y,f) = f y
  η0 x    = \y → (y,x)
  φl = undefined
  φr = undefined

instance d ~ e ⇒ Monad (Prod d) (Hom e)

data State s a = State (Prod s a) (Hom s a)
