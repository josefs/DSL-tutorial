% Combining Deep and Shallow Embeddings: A Tutorial
% Josef Svenningsson
%

# Combining Deep and Shallow Embeddings

# Background

The ideas I'm going to present in this tutorial have been used
extensively in the development of Feldspar.

Feldspar is a domain specific language 

Feldspar is a collaboration between:

----                               ----
Chalmers University of Technology  ![Chalmers logo]
ELTE University                    ![ELTE logo]
Ericsson                           ![Ericsson logo]
----                               ----

[Chalmers logo]: Chalmers_logo.png
[ELTE logo]: cimer.png
[Ericsson logo]: 219px-Ericsson_logo.png

# Contrast Deep and Shallow Embeddings

Running example contrasting deep and shallow

Simple arithmetic language

~~~
(+) :: SInt -> SInt -> SInt
(*) :: SInt -> SInt -> SInt
5 :: SInt
~~~

# Example Embedding

# Deep Embedding

# Shallow Embedding

# Hybrid approaches

In Kansas Lava they use a hybrid approach where the deep embedding contains
a shallow embedding.

~~~
data Signal a = Signal (Stream a) (D a)

data Stream a = a :~ Stream a

type D a = AST
~~~

# Conclusion: Deep and Shallow embeddings

Deep and Shallow embeddings are two important ways to embed a target language
in a host language

These are certainly not the only ways to embed a language, but they will be
the focus in this tutorial.

# Main example

# Functional C -- Deep Embedding

Our example language for demonstrating our main point is a small
functional flavored C.

It is inspired by the core language used in Feldspar, which has served us well
for writing embedded software.


# Various Shallow Embeddings

# Vectors

# Sequential Vectors

# Streams

# Pairs

# Complex Numbers

A good example of where we experimented with a feature using the shallow
embedding but then included it in the deep embedding.


A good example of where we experimented with a feature using the shallow
embedding but then included it in the deep embedding.


# Fixed Point Numbers

# Type Class for seamless mixing

A type class which summarizes all the language features -- both shallowly
and deeply embedded

# Partial Evaluation

The host language is effectively the meta language of the object language.

Haskell values are static.

DSL values are dynamic.

Should I show the Binding Time module?

# Analysis and optimization of shallow embeddings

We can optimize vectors even further if we know something about their shape.

Show example with `(++)`.


[Final:ish slide]
# Finally Tagless

Why haven't I said anything about the Finally Tagless approach. The
Finally Tagless approach solves some of the same problems that I've
talked about in this tutorial.

* Problems with weird types involving type variables and type constraints
* Not nice for end users who are not themselves functional programmers
* It doesn't mix well with observable sharing

# Thanks

Thanks!