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

[Chalmers logo]: Chalmers_logo.png
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

What do I mean with Deep and Shallow Embeddings

# Running example contrasting Deep and Shallow Embeddings

Simple language for regions

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

Credit to Paul Hudak

# Shallow Embedding

~~~
type Region = Point -> Bool

p `inRegion` r = r p

circle  r = \p -> magnitude p <= r
outside r = \p -> not (r p)
r1 /\ r2  = \p -> r1 p && r2 p
r1 \/ r2  = \p -> r1 p || r2 p
~~~

# Shallow Embedding -- Pros and Cons

## Pros

* Easy to add new language constructs 
  -- as long as they can be interpreted in the semantic domain

* Efficient, tagless evaluation

##  Cons

* Fixes the interpretation, not possible to (easily) add another interpretation

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

# Deep Embedding -- Pros and Cons

## Pros

* Easy to add new forms of interpretations

  E.g. compile regions to C functions

* Possible to optimize the representation

  ~~~~
  outside (Outside r) = r
  ~~~~

## Cons

* Laborious to add new language constructs

* Requires a new data type for the AST -- more code

* Potentially slow, tagful interpretation

# Embeddings - Pros and Cons

-----  -----                                 -----
       Shallow                               Deep
-----  -----                                 -----
Pros   * Easy to add new language 
         constructs 
         -- as long as they can be 
         interpreted in the semantic domain  
       * Efficient, tagless evaluation

Cons   * Fixes the interpretation, not       * Laborious to add new language 
         possible to (easily) add              constructs
	 another interpretation              * Potentially slow, tagful 
					       interpretation
                                             * Requires a new data type for 
                                               the AST -- more code
-----  ----                                  ----

# Hybrid approaches

Kansas Lava, an embedded hardware description language, employs a
hybrid approach where the deep embedding contains a shallow embedding.

~~~
data Signal a = Signal (Stream a) (D a)

data Stream a = a :~ Stream a

type D a = AST
~~~

## Pros

Allows for both efficient, tagless evaluation and computing an AST

## Cons

Worst of both world when it comes to extensibility

# Conclusion: Deep and Shallow embeddings

Deep and Shallow embeddings are two important ways to embed a target language
in a host language

Use a shallow embedding:

* when prototyping
* when only one interpretation is needed
* if interpretation needs to be tagless

Use a deep embedding:

* when multiple interpretations are required
* when compiling to language

# Finally Tagless

The Finally Tagless approach to embedding languages solves some of the
same problems that I'm going to talk about in this tutorial.

Why I not going to say anything about the Finally Tagless approach, and
why we don't use it in the Feldspar project:

* Types become unfriendly, involving e.g higher kinded type variables
* Not nice for end users who are not themselves functional programmers
* Does not mix well with observable sharing

# Combining Deep and Shallow Embedding

The essence of this technique is to have:

* A deeply embedded core language
* Additional language features shallowly embedded

# Functional C -- Deep Embedding

Our example language for demonstrating our main point is a small
functional flavored C.

~~~
data FunC a where
  LitI   :: Int  -> FunC Int
  LitB   :: Bool -> FunC Bool
  While :: (FunC s -> FunC Bool) -> (FunC s -> FunC s) -> FunC s -> FunC s
  If    :: FunC Bool -> FunC a -> FunC a -> FunC a

  Prim1    :: String -> (a -> b) -> FunC a -> FunC b
  Prim2    :: String -> (a -> b -> c) -> FunC a -> FunC b -> FunC c

  Value    :: a -> FunC a
  Variable :: String -> FunC a
~~~


It is inspired by the core language used in Feldspar, which has served us well
for writing embedded software.

The fact that we're using HOAS is inconsequential for this tutorial.

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
ifC :: Syntactic a => FunC Bool -> a -> a -> a
ifC c t e = fromFunC (If c (toFunC t) (toFunC e))

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

# Adding a feature

Under our new regiment, new language features come with a Deep part
and a Shallow part.

The Deep part is an addition to the AST

The Shallow part is a new type

# Vector feature

Our FunC language is rather impoverished at the moment and at the very
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
~~~

This is a popular representation for arrays. Can be traced back to Pan

Arrays will be stored in memory

Each element is computed independently; can be computed in parallel

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
mapVec :: (a -> b) -> Vector a -> Vector b
mapVec f (Indexed l ixf) = Indexed l (f . ixf)

takeVec :: FunC Int -> Vector a -> Vector a
takeVec i (Indexed l ixf) = Indexed (min i l) ixf
~~~

Note that they only involve the Shallow embedding, no manipulation
of syntax trees

# Vectors cont.

A consequence of this design is that the `Vector` type only exists at
compile time, i.e. during Haskell evaluation.

Once the syntax tree is built there are no `Vector` values left

This is usually referred to as *Fusion*.

# Fusion with Vectors

With our design of `Vector` we can give strong guarantees about fusion

> Whenever two vector functions are composed, the intermediate vector
> is removed.

Much stronger guarantee than conventional compiler optimizations

Helps keep the cost model transparent

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

# Pros and Cons

## Pros

* Adding new features is relatively light weight by letting the shallow
  embedding do most of the work
* Fusion

## Cons

* We have to modify the AST

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
* Write some functions for the new feature, like `map` and `scan`
* Does the new representation support fusion? Experiment!

# Credits

I can take very little credit for the presented material

* Emil Axelsson as written most of the core Feldspar code
* The rest of the Feldspar team for discussion
* Pan, by Elliott, Finne & de Moor, contained many of the ideas presented here

# Thanks

Thanks!
