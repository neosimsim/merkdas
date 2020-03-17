and : Bool → Bool → Bool
and = λ { true x → x ; false _ → false }

xor : Bool → Bool → Bool
xor = λ { true  true  → false
        ; false false → false
        ; _     _     → true
        }

fst : {A : Set} {B : A → Set} → Σ A B → A
fst = λ { (a , b) → a }

snd : {A : Set} {B : A → Set} (p : Σ A B) → B (fst p)
snd = λ { (a , b) → b }

zero = 0
