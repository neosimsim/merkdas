# Bool

```
module Bool where
```

Now we know Agda works let's get our hands dirty by defining some data types
and functions.

I can't thing of many simplier data types than `Bool`, so let's start there.

```
data Bool : Set where
  true  : Bool
  false : Bool
```

What's that? This is an algebraic data type similar to those we wrote and use
in Haskell all day long, or at least we would if we could use Haskell all day
long at work.

    data Bool = True | False

If we write the same with Haskells GADTs extension we see the resemblanse:

    data Bool where
      True  :: Bool
      False :: Bool

So far so good, but what about `Set`? Whe state that `Bool` is of type
`Set`, which is the type of small types. In Haskell we would say of kind `*`.
`Set` itself is of type `Set1` which is of type `Set2` and so on. In Haskell
we would say nothing like this.

    data Bool :: * where
       True  :: Bool
       False :: Bool

In other word beeing a Type in Haskell means beeing a Set in Agda.

## First function
Based on out new type we can write some function. How about if-then-else?

```
if_then_else_ : {A : Set} → Bool → A → A → A
if true then x else _ = x
if false then _ else y = y
```
