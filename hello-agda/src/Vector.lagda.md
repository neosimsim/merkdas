# Fixed Length Vector

```
module Vector where

open import Data.Nat using (ℕ; suc)
open import Agda.Builtin.Float
  using (Float; primFloatPlus; primFloatTimes)

data Vec (A : Set) : ℕ → Set where
  []   : Vec A 0
  Cons : (n : ℕ) → A → Vec A n → Vec A (suc n)
```
So given a known length, a value and a known vector we get a vector
with the value added and 1 longer.

Let's define some vectors
```
nil₁ : Vec ℕ 0
nil₁ = []


gravity₁ : Vec Float 3
gravity₁ = Cons 2 0.0 (Cons 1 0.0 (Cons 0 -9.81 []))
```

Once again this turns out to be a bit tedious. We have to pass the length
of each intermediate vector, which if you think about should be known to agda.
If Agda can infer the arguments we can make them implicit by using curly braces.
So lets change the definition of vector
```
infixr 10 _<>_
data Vector (A : Set) : ℕ → Set where
  ∅    : Vector A 0
  _<>_ : {n : ℕ} → A → Vector A n → Vector A (suc n)
```
I also made `Cons` an infix operator. And here are our vectors again
```
nil₂ : Vector ℕ 0
nil₂ = ∅

gravity₂ : Vector Float 3
gravity₂ = 0.0 <> 0.0 <> -9.81 <> ∅
```

With fixed length vectors we can already write fascinating type safe code.

```
head : {A : Set} {n : ℕ} → Vector A (suc n) → A
head (x <> _) = x
```

Note that this compiles, although we did not pattern match on `∅`.
We don't have too, in fact we can't pattern match on `∅`
since `∅` construct a vector with index 0, which is no successor
to any natural number, remember?

\begin{exercise}
User \verb+primFloatPlus+ to define vector addition on vectors of \verb+Float+s
\end{exercise}

```
infixr 6 _+v_
_+v_ : {n : ℕ}
     → Vector Float n
     → Vector Float n
     → Vector Float n
∅         +v _         = ∅
(v <> vs) +v (w <> ws) = (primFloatPlus v w) <> (vs +v ws)
```
Again we don't have to care about the case where the length of the vectors
don't match.

```
foo : Vector Float 3
foo = gravity₂ +v gravity₂
```
No more out of bounds exceptions.

\begin{exercise}
Implement the scalar product on vectors
\end{exercise}

```
infixr 7 _×_
_×_ : {n : ℕ}
    → Vector Float n
    → Vector Float n
    → Float
∅         × _         = 0.0
(v <> vs) × (w <> ws) = primFloatPlus (primFloatTimes v w) (vs × ws)
```

Finally
\begin{exercise}
Define length of the vector.
\end{exercise}

This is tricky since we use the implicit argument which wasn't introduced, yet.


We can implement `length` by recursion, but that does not seem right. We know the
length of the vector at compile time. Why should it be neccessary to calculate the
length at runtime? Fortunately it is not. Although we pass the type `A` and the
length `n` as implicit arguments we can still access them in the function definition.
```
length : {A : Set} {n : ℕ} → Vector A n → ℕ
length {_} {n} _ = n
```
