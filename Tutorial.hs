{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax  #-}
module Tutorial where

import Prelude hiding ((>=),(-),subtract,fromInteger)
import qualified Prelude

import Data.Array

data FunC a where
  LitI   :: Int  -> FunC Int
  LitB   :: Bool -> FunC Bool
  While :: (FunC s -> FunC Bool) -> (FunC s -> FunC s) -> FunC s -> FunC s
  If    :: FunC Bool -> FunC a -> FunC a -> FunC a
  Pair  :: FunC a -> FunC b -> FunC (a,b)
  Fst   :: FunC (a,b) -> FunC a
  Snd   :: FunC (a,b) -> FunC b

  Prim1    :: String -> (a -> b) -> FunC a -> FunC b
  Prim2    :: String -> (a -> b -> c) -> FunC a -> FunC b -> FunC c

  Value    :: a -> FunC a
  Variable :: String -> FunC a

  Arr   :: FunC Int -> (FunC Int -> FunC a) -> FunC (Array Int a)


eval :: FunC a -> a
eval (LitI i) = i
eval (LitB b) = b
eval (Arr l ixf) = listArray (zero,lm1) $
                   map (eval . ixf . value) [zero..lm1]
  where lm1  = eval l Prelude.- succ zero
        -- Fix me. I should probably separate this code with the examples
        -- so that I don't need RebindableSyntax when defining this function
        zero = lm1 Prelude.- lm1
eval (While c b i) = head $
                     dropWhile (eval . c . value) $
                     iterate (eval . b . value) $
                     eval i
eval (If c t e)  = if eval c then eval t else eval e
eval (Pair a b)  = (eval a, eval b)
eval (Fst p)     = fst (eval p)
eval (Snd p)     = snd (eval p)
eval (Prim1 _ f a) = f (eval a)
eval (Prim2 _ f a b) = f (eval a) (eval b)
eval (Value a)   = a

value = Value

ifC :: Syntactic a => FunC Bool -> a -> a -> a
ifC c t e = fromFunC (If c (toFunC t) (toFunC e))

while :: Syntactic s => (s -> FunC Bool) -> (s -> s) -> s -> s
while c b i = fromFunC (While (c . fromFunC) (toFunC . b . fromFunC) (toFunC i))

class Syntactic a where
  type Internal a
  toFunC   :: a -> FunC (Internal a)
  fromFunC :: FunC (Internal a) -> a

instance Syntactic (FunC a) where
  type Internal (FunC a) = a
  toFunC   ast = ast
  fromFunC ast = ast

-- Shallow Embeddings

-- Complex Numbers

data Complex = Complex (FunC Float) (FunC Float)

instance Syntactic Complex where
  type Internal (Complex) = (Float,Float)
  toFunC (Complex i r) = Pair i r
  fromFunC p = Complex (Fst p) (Snd p)

-- Vectors

data Vector a where
  Indexed :: FunC Int -> (FunC Int -> a) -> Vector a

instance Syntactic a => Syntactic (Vector a) where
  type Internal (Vector a) = Array Int (Internal a)
  toFunC (Indexed l ixf)   = Arr l (toFunC . ixf)
  fromFunC arr = Indexed (len arr) (\ix -> arr +!+ ix)

len :: FunC (Array Int a) -> FunC Int
len arr = Prim1 "length" (uncurry Prelude.subtract . bounds) arr

lenV :: Vector a -> FunC Int
lenV (Indexed l _) = l

(+!+) :: Syntactic a => FunC (Array Int (Internal a)) -> FunC Int -> a
arr +!+ ix = fromFunC (Prim2 "index" (!) arr ix)

mapVec :: (a -> b) -> Vector a -> Vector b
mapVec f (Indexed l ixf) = Indexed l (f . ixf)

takeVec :: FunC Int -> Vector a -> Vector a
takeVec i (Indexed l ixf) = Indexed (minF i l) ixf

minF :: FunC Int -> FunC Int -> FunC Int
minF a b = Prim2 "min" min a b

-- Interesting variation: statically sized vectors

data SVector a where
  SVector :: Int -> (FunC Int -> a) -> SVector a

-- Streams

data Stream a where
  Stream :: (s -> (a,s)) -> s -> Stream a

-- Finite stream (sequential vectors)

data FStream a where
  FStream :: FunC Int -> (s -> (a,s)) -> s -> FStream a

data SFStream a where
  SFStream :: Int -> (s -> (a,s)) -> s -> SFStream a

-- Fixed point numbers with static exponent
{-
data FixedPoint = FP Int (FunC Int)

instance Num FixedPoint where
  FP e1 m1 + FP e2 m2 | e1 == e2 = FP e1 (m1 + m2)
  FP e1 m1 - FP e2 m2 | e1 == e2 = FP e1 (m1 - m2)
  FP e1 m1 * FP e2 m2 | e1 == e2 = FP e1 ((m1 * m2) shift 
-}

(>=) :: FunC Int -> FunC Int -> FunC Bool
a >= b = Prim2 "(>=)" (Prelude.>=) a b

(-) :: FunC Int -> FunC Int -> FunC Int
a - b = Prim2 "(-)" (Prelude.-) a b

subtract :: FunC Int -> FunC Int -> FunC Int
subtract b a = a - b

fromInteger :: Integer -> FunC Int
fromInteger i = LitI (Prelude.fromInteger i)

ifThenElse :: Bool -> a -> a -> a
ifThenElse True  t _ = t
ifThenElse False _ e = e

-- Example programs

modulus :: FunC Int -> FunC Int -> FunC Int
modulus a b = while (>=b) (subtract b) a

modulusTest = eval (modulus 17 2)
