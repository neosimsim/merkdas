module Logic where

data Bool : Set where
  true : Bool
  false : Bool

_∧_ : Bool → Bool → Bool
false ∧ _ = false
true ∧ b = b 

_∨_ : Bool → Bool → Bool
true ∨ _ = true
false ∨ b = b 