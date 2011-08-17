% Combining Deep and Shallow Embeddings
% Josef Svenningsson
%


# Introduction

The advantages of embedding domain specific languages are well known.
It helps the language implementor(s) focus on language design by
leveraging the parser and (when applicable) the type checker of the
host language.

When embedding a language there are several techniques to choose from
which are typically categorized as *shallow* and *deep*
embeddings. These techniques have their individual merits and are
often considered separate.

In this tutorial we will show that much can be gained from combining
deep and shallow embeddings. More concretely we will show the benefits
of starting with a deep embedding and adding language constructs which
are implemented using shallow embeddings on top of the deep embedding.

## Terminology

Before getting to the meat of this tutorial we will establish some
terminology.

We will use *object language* to refer to the DSL which is being
embedded.  In this tutorial we will see two examples of object
languages. The term *host language* will be used to refer to the
language which the object language is embedded in. We will use Haskell
as our host language throughout this tutorial.

# Contrast Deep and Shallow Embeddings

Before delving into the subject of combining deep and shallow
embeddings let be precise about what those terms mean and the way that
we will use them in tutorial.

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

# Main example

# Functional C -- Deep Embedding

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

# Other hybrid approaches

There are other approaches to combining deep and shallow embeddings, not to be
confused with the one presented in this tutorial.

In the implementation of Kansas Lava they store shallow values in the
syntax tree for easy access [@Farmer:10:WhatsTheMatter].

In the Feldspar project we do something similar but 

## Finally Tagless

The Finally Tagless approach is an alternative method to embed
languages. It has a lot of the same advantages as the approach we have
presented in this tutorial and can in some cases even be considered
superiour.


Why haven't I said anything about the Finally Tagless approach. The
Finally Tagless approach solves some of the same problems that I've
talked about in this tutorial.

* Problems with weird types involving type variables and type constraints
* Not nice for end users who are not themselves functional programmers
* It doesn't mix well with observable sharing

# References
