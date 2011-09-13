% Combining Deep and Shallow Embeddings: A Tutorial
% Josef Svenningsson
%

# Background

The ideas I'm going to present in this tutorial have been used
extensively in the development of Feldspar.

Feldspar is a collaboration between:

----                               ----
Ericsson                           ![Ericsson logo]
Chalmers University of Technology  ![Chalmers logo]
ELTE University                    ![ELTE logo]
----                               ----

[Chalmers logo]: Chalmers_logo_small.png
[ELTE logo]: cimer.png
[Ericsson logo]: 219px-Ericsson_logo.png

# Feldspar

Feldspar is a domain specific language for programming low-level,
performance sensitive embedded systems with focus on digital signal
processing.

* Embedded in Haskell
* Strongly typed
* Style of programming is similar to Haskell
* Transparent cost model
* Compiles to C, an LLVM backend is being worked on
* Datapath layer is Pure, upcoming version will feature monads
* A system layer in the works which deals with orchestrating computations

# Contrast Deep and Shallow Embeddings

What do I mean with Deep and Shallow Embeddings?

# Running example contrasting Deep and Shallow Embeddings

Simple language for 2D regions

~~~
type Region

inRegion :: Point -> Region -> Bool

circle  :: Radius -> Region
outside :: Region -> Region
(/\)    :: Region -> Region -> Region
(\/)    :: Region -> Region -> Region

annulus :: Radius -> Radius -> Region
annulus r1 r2 = outside (circle	r1) /\ (circle r2)
~~~

Credit to Carlsson, Hudak and Jones \cite{carlson1993experiment}

# Shallow Embedding

~~~
type Region = Point -> Bool

p `inRegion` r = r p

circle  r = \p -> magnitude p <= r
outside r = \p -> not (r p)
r1 /\ r2  = \p -> r1 p && r2 p
r1 \/ r2  = \p -> r1 p || r2 p
~~~

The type `Point -> Bool` is the *semantic domain* of the shallow embedding

# Shallow Embedding - Pros and Cons

## Pros

* Easy to add new language constructs - as long as they can be
  interpreted in the semantic domain

* Efficient, tagless evaluation

##  Cons

* Fixes the interpretation, doesn't allow for adding another interpretation

# Deep Embedding

~~~
data Region = Circle    Radius
            | Outside   Region
            | Intersect Region Region
            | Union     Region Region

circle  r = Circle    r
outside r = Outside   r
r1 /\ r2  = Intersect r1 r2
r1 \/ r2  = Union     r1 r2

p `inRegion` (Circle  r)       = magnitude p <= r
p `inRegion` (Outside r)       = not (p `inRegion` r)
p `inRegion` (Intersect r1 r2) = p `inRegion` r1 && p `inRegion` r2
p `inRegion` (Union     r1 r2) = p `inRegion` r1 || p `inRegion` r2
~~~

# Deep Embedding - Pros and Cons

## Pros

* Easy to add new forms of interpretations

  E.g. compile regions to C functions

* Possible to optimize the representation, e.g. using smart constructors

  ~~~~
  outside (Outside r) = r
  ~~~~

## Cons

* Laborious to add new language constructs

* Requires a new data type for the AST - more code

* Potentially slow, tagful evaluation

# Conclusion: Deep and Shallow embeddings

Deep and Shallow embeddings are two complementary ways to embed a
target language in a host language

Use a shallow embedding:

* when prototyping
* when only one interpretation is needed
* if interpretation needs to be tagless

Use a deep embedding:

* when multiple interpretations are required
* when compiling to language

# Finally Tagless

The Finally Tagless approach \cite{carette2009finally} to embedding
languages can be used to achieve effects similar to what I present
here.

Why I not going to say anything about the Finally Tagless approach, and
why we don't use it in the Feldspar project:

* Types become a little less unfriendly, involving e.g higher kinded type variables
* Hard to motivate for end users who are not themselves functional programmers

# Combining Deep and Shallow Embedding

Goals:

* Nice interface for the embedded language
* Make it convenient to experiment with new language features

# Combining Deep and Shallow Embedding

The essence of this technique is to have:

* A deeply embedded core language
* Additional language features shallowly embedded
* The deep embedding becomes the semantic domain of the shallow embeddings

# Functional C - Deep Embedding

Our example language for demonstrating our main point is a small
functional flavored C.

~~~
data FunC a where
  LitI     :: Int  -> FunC Int
  LitB     :: Bool -> FunC Bool
  While    :: (FunC s -> FunC Bool) -> (FunC s -> FunC s) -> FunC s -> FunC s
  If       :: FunC Bool -> FunC a -> FunC a -> FunC a
  Pair     :: FunC a -> FunC b -> FunC (a,b)
  Fst      :: FunC (a,b) -> FunC a
  Snd      :: FunC (a,b) -> FunC b
  
  Prim1    :: String -> (a -> b) -> FunC a -> FunC b
  Prim2    :: String -> (a -> b -> c) -> FunC a -> FunC b -> FunC c

  Value    :: a -> FunC a
  Variable :: String -> FunC a
~~~


It is inspired by the core language used in Feldspar, which has served us well
for writing embedded software.

The problem of having several constructor for primitive functions is
solvable, but that's a topic for another tutorial

# The meaning of `FunC`

The semantics of `FunC`

~~~
eval :: FunC a -> a
eval (LitI i)        = i
eval (LitB b)        = b
eval (While c b i)   = head $
                       dropWhile (eval . c . value) $
                       iterate (eval . b . value) $
                       eval i
eval (If c t e)      = if eval c then eval t else eval e
eval (Pair a b)      = (eval a,eval b)
eval (Fst p)         = fst (eval p)
eval (Snd p)         = snd (eval p)
eval (Prim1 _ f a)   = f (eval a)
eval (Prim2 _ f a b) = f (eval a) (eval b)
eval (Value a)       = a
~~~

# Syntactic class

In order to integrate additional types to the language in a smooth way we
introduce the following type class.

~~~
class Syntactic a where
  type Internal a
  fromFunC :: FunC (Internal a) -> a
  toFunC   :: a -> FunC (Internal a)
~~~

This type class summarises all types which can be used in our domain
specific language. The functions `fromFunC` and `toFunC` shows how to
convert to and from our AST representation.

# Syntactic instance

The Syntactic instance for `FunC`

~~~
instance Syntactic (FunC a) where
  type Internal (FunC a) = a
  toFunC   ast = ast
  fromFunC ast = ast
~~~

# Rephrasing Deeply Embedded Features

Given the `Syntactic` class we now export all our language constructs
as overloaded

~~~
true :: FunC Bool
true = LitB True

false :: FunC Bool
false = LitB False

ifC :: Syntactic a => FunC Bool -> a -> a -> a
ifC c t e = fromFunC (If c (toFunC t) (toFunC e))

(?) = ifC

while :: Syntactic s => (s -> FunC Bool) -> (s -> s) -> s -> s
while c b i = fromFunC (While (c . fromFunC) (toFunC . b . fromFunC) (toFunC i))
~~~

The types of these overloaded functions are very Haskell-like

Allows for easily extending the language, by instantiating `Syntactic`

Makes programming in the embedded language quite a bit more pleasant.

Base types are still on the form `FunC a`

# Example Program

A small program computing the modulus by means of iterated subtraction

~~~
modulus :: FunC Int -> FunC Int -> FunC Int
modulus a b = while (>=b) (subtract b) a
~~~

I assume the existence of primitive functions `>=` and `subtract`

# Adding a feature

Under our new regime, new language features are represented using a
Shallow embedding.

The Shallow embedding is a new type

If the Deep embedding cannot represent the new language feature we
will have to extend the AST

# Options feature

As a warmup we will add conditional values to `FunC`.

The idea is to mimic provide functionality similar to Haskell's
`Maybe`-type.

~~~
data Maybe a = Just a | Nothing
~~~

# Option embedding

~~~
data Option a = Option { isSome   :: FunC Bool
                       , fromSome :: a
                       }

instance Syntactic a => Syntactic (Option a) where
  type Internal (Option a) = (Bool,Internal a)
  fromFunC m = Option (Fst m) (Snd m)
  toFunC (Option b a) = Pair b a
~~~

The name `Option` comes from O`Caml and is chosen so as to not collide with
any Haskell type.

# Operations on Option - 1st attempt

~~~
some :: a -> Option a
some a = Option true a

none :: Option a
none = Option false ?
~~~

How can we define `none`?

We need a notion of undefined values!

# Extending the deep embedding with undefined values

~~~
data FunC
  ...
  Undef :: FunC a
  ...

eval (Undef) = undefined

undef :: Syntactic a => a
undef = fromFunC Undef
~~~


# Operations on Option

~~~
some :: a -> Option a
some a = Option true a

none :: Syntactic a => Option a
none = Option false undef

option :: (Syntactic a, Syntactic b) => 
          b -> (a -> b) -> Option a -> b
option noneCase someCase opt = ifC (isSome opt)
       	                           (someCase (fromSome opt))
                                   noneCase

instance Functor Option where
  fmap f (Option b a) = Option b (f a)

instance Monad Option where
  return a  = some a
  opt >>= k = b { isSome = isSome opt ? (isSome b, false) }
    where b = k (fromSome opt)
~~~

# Example program

Assuming the following operation

~~~
divO :: FunC Int -> FunC Int -> Option (FunC Int)
~~~

We can use Haskell's overloaded do-notation

~~~
divTest :: FunC Int -> FunC Int -> FunC Int -> Option (FunC Int)
divTest a b c = do r1 <- divO a b
                   r2 <- divO a c
                   return (a+b)
~~~

The resulting AST is a sequence of if-expressions.

# Vector feature

Our `FunC` language is rather impoverished at the moment and at the very
minimum we will require arrays.

In our addition we will distinguish between *arrays* and *vectors*

* Arrays will be our Deep representation
* Vectors will be our Shallow representation

The idea is that programmers should not worry about Arrays but only
program with Vectors.

# Vector feature - arrays

As our Deep representation we have arrays.

~~~
data FunC a where
  ...
  Arr   :: FunC Int -> (FunC Int -> FunC a) -> FunC (Array Int a)
  ...

eval (Arr l ixf) = listArray (0,lm1) $
                   [ (eval $ ixf $ value i) | i  <- [0..lm1]]
  where lm1  = eval l - 1

len :: FunC (Array Int a) -> FunC Int
len (Arr l _) = l

(+!+) :: FunC (Array Int a) -> FunC Int -> a
Arr _ f +!+ ix = f ix
~~~

This is a popular representation for arrays. Can be traced back to Pan

Arrays will be stored in memory

Each element is computed independently; the whole array can be
computed in parallel

# Vectors cont.

As our Shallow embedding we have the type `Vector`

~~~
data Vector a where
  Indexed :: FunC Int -> (FunC Int -> a) -> Vector a

instance Syntactic a => Syntactic (Vector a) where
  type Internal (Vector a) = Array Int (Internal a)
  toFunC (Indexed l ixf)   = Arr l (toFunC . ixf)
  fromFunC arr = Indexed (len arr) (\ix -> arr +!+ ix)
~~~

Note the similarity with the Array constructor

The `Syntactic` instance makes it part of our language

# Vectors - example functions

Examples of how to write functions for `Vector`

~~~
takeVec :: FunC Int -> Vector a -> Vector a
takeVec i (Indexed l ixf) = Indexed (minF i l) ixf

enumVec :: FunC Int -> FunC Int -> Vector (FunC Int)
enumVec f t = Indexed (t - f + 1) (\ix -> ix + f)

instance Functor Vector where
  fmap f (Indexed l ixf) = Indexed l (f . ixf)
~~~

Note that they only involve the Shallow embedding, no manipulation
of syntax trees

# Vectors - Example program

Compute the moving average of a vector

~~~
movingAvg :: FunC Int -> Vector (FunC Int) -> Vector (FunC Int)
movingAvg n = map ((`divP` n) . sum . take n) . tails
~~~

# Vectors cont.

A consequence of this design is that the `Vector` type only exists at
compile time, i.e. during Haskell evaluation.

Once the syntax tree is built there are no `Vector` values left

This is usually referred to as *Fusion*.

# Example of Fusion

A small test program

~~~
squares :: FunC Int -> Vector (FunC Int)
squares n = fmap square $ enumVec 1 n
  where square x = x * x
~~~

The intermediate vector will disappear

~~~
\a -> { (i+1) * (i+1) | i <- 0 .. (a + 1) - 1 }
~~~

This made-up syntax is an array comprehension. The variable `i` takes on
all the indices of the array, and the expression `(i+1) * (i+1)` computes
the value of the array for each index.

# Fusion with Vectors

With our design of `Vector` we can give strong guarantees about fusion

> Whenever two vector functions are composed, the intermediate vector
> is removed.

Much stronger guarantee than conventional compiler optimizations

Helps the programmer understand whether his program will be efficient or not.

# Fusion caveat

One caveat that is easy to forget

> When Vectors are passed around in loops, they will be stored written to
> memory on each iteration.

# Monads

If we are defining a pure language, it is often that we still want
side-effects,

* for efficiency
* on the top level to orchestrate computations
* communicate with the environment.

Monads is the standard solution to this problem and it is perfectly
possible to add monads to an embedded language using the techniques
presented here. There is a paper at IFL this year describing the
details.

# Combining Deep and Shallow Embeddings - Pros and Cons

## Pros

* Adding new features is relatively light weight by letting the shallow
  embedding do most of the work
  - Typically a single constructor suffices to cover a lot of functionality
* Very Haskell-like programming interface for the embedded language
  - Supports writing instances for many common classes and using Haskell's
    built-in syntactic sugar
* Fusion

## Cons

* We sometimes have to modify the AST data type

## Remedy

* Use something like "Datatype A La Carte" to make it easier to add
  features to the AST. We have a library "Syntactic" which does that
  and more.

# Excercise

One problem with our representation of vectors is that some functions
are very inefficient to compute. For example *scans*.

The core of this problem is that each element is computed
independently, we cannot pass any values between these computations.

A better representation would be to base it on the type of array `unfold`

~~~
unfold :: (b -> (a,b)) -> b -> Int -> Array Int a
~~~

# Excercise - cont.

Excercise:

* Add a new constructor to the AST to represent sequential arrays
* Write a new type, or add a constructor to the already existing `Vector` type
* Write the `Syntactic` instance
* Write some functions for the new feature, like `map` and `scan`.
* Does the new representation support fusion? Experiment!

# Credits

I can take very little credit for the presented material

* Emil Axelsson as written most of the core Feldspar code
* The rest of the Feldspar team for discussion
* Pan, by Elliott, Finne & de Moor, contains many of the ideas presented here \cite{elliott2003compiling}

# Thanks

Thanks!
