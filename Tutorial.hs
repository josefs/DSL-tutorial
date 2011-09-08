{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}
module Tutorial where

import Prelude hiding (Ord(..),Eq(..))
import qualified Prelude

import Data.Array

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

  Arr      :: FunC Int -> (FunC Int -> FunC a) -> FunC (Array Int a)
  Undef    :: FunC a

  Value    :: a -> FunC a
  Variable :: String -> FunC a


eval :: FunC a -> a
eval (LitI i)    = i
eval (LitB b)    = b
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

instance Show (FunC a) where
  show (LitI i) = show i
  show (LitB b) = show b
  show (While c b i) = "while (s = " ++  show i ++ ";" ++ 
                          show (c (Variable "s")) ++  ") \n" ++
                       "  s = " ++ show (b (Variable "s"))
  show (If c t e) = "if " ++ show c ++ 
                    " then " ++ show t ++
                    " else " ++ show e
  show (Pair a b) = "(" ++ show a ++ "," ++ show b ++ ")"
  show (Fst p) = "fst " ++ show p
  show (Snd p) = "snd " ++ show p
  show (Prim1 f _ a) = f ++ " (" ++ show a ++ ")"
  show (Prim2 f _ a b) = f ++ " (" ++ show a ++ ") (" ++ show b ++ ")"
  show (Arr l ixf) = "{ " ++ show (ixf (Variable "i")) ++ 
                     " | i <- 0 .. " ++ show (l-1) ++ " }"
  show (Undef) = "undefined"
  show (Variable v) = v

instance (Syntactic a, Show b) => Show (a -> b) where
  show f = "\\ a -> " ++ show (f (fromFunC (Variable "a")))

-- Nicer Interface

class Syntactic a where
  type Internal a
  toFunC   :: a -> FunC (Internal a)
  fromFunC :: FunC (Internal a) -> a

instance Syntactic (FunC a) where
  type Internal (FunC a) = a
  toFunC   ast = ast
  fromFunC ast = ast

true :: FunC Bool
true = LitB True

false :: FunC Bool
false = LitB False

undef :: Syntactic a => a
undef = fromFunC Undef

ifC :: Syntactic a => FunC Bool -> a -> a -> a
ifC c t e = fromFunC (If c (toFunC t) (toFunC e))

c ? (t,e) = ifC c t e

while :: Syntactic s => (s -> FunC Bool) -> (s -> s) -> s -> s
while c b i = fromFunC (While (c . fromFunC) (toFunC . b . fromFunC) (toFunC i))

instance Prelude.Eq (FunC Int) where

instance Num (FunC Int) where
  (+) = Prim2 "(+)" (+)
  (-) = Prim2 "(-)" (-)
  (*) = Prim2 "(*)" (*)
  negate = Prim1 "negate" negate
  abs = Prim1 "abs" abs
  signum = Prim1 "signum" signum
  fromInteger = LitI . fromInteger

(==) :: FunC Int -> FunC Int -> FunC Bool
(==) = Prim2 "(==)" (Prelude.==)

-- Shallow Embeddings

-- Pairs

instance (Syntactic a, Syntactic b) => Syntactic (a,b) where
  type Internal (a,b) = (Internal a, Internal b)
  toFunC (a,b) = Pair (toFunC a) (toFunC b)
  fromFunC p = (fromFunC (Fst p), fromFunC (Snd p))

pair :: (Syntactic a, Syntactic b) =>
        a -> b -> (a,b)
pair a b = fromFunC (Pair (toFunC a) (toFunC b))

fstP :: (Syntactic a, Syntactic b) =>
        (a,b) -> a
fstP (a,_) = a

sndP :: (Syntactic a, Syntactic b) =>
        (a,b) -> b
sndP (_,b) = b

-- Option

data Option a = Option { isSome   :: FunC Bool
                       , fromSome :: a
                       }

instance Syntactic a => Syntactic (Option a) where
  type Internal (Option a) = (Bool,Internal a)
  fromFunC m = Option (Fst m) (fromFunC (Snd m))
  toFunC (Option b a) = Pair b (toFunC a)

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

divO :: FunC Int -> FunC Int -> Option (FunC Int)
divO a b = (b == 0) ? (none, some (a `divP` b))

divP :: FunC Int -> FunC Int -> FunC Int
divP a b = Prim2 "div" div a b

-- Vectors

data Vector a where
  Indexed :: FunC Int -> (FunC Int -> a) -> Vector a

instance Syntactic a => Syntactic (Vector a) where
  type Internal (Vector a) = Array Int (Internal a)
  toFunC (Indexed l ixf)   = Arr l (toFunC . ixf)
  fromFunC arr = Indexed (len arr) (\ix -> arr +!+ ix)

instance Syntactic a => Show (Vector a) where
  show = show . toFunC

len :: FunC (Array Int a) -> FunC Int
len arr = Prim1 "length" (uncurry subtract . bounds) arr

lenV :: Vector a -> FunC Int
lenV (Indexed l _) = l

(+!+) :: Syntactic a => FunC (Array Int (Internal a)) -> FunC Int -> a
arr +!+ ix = fromFunC (Prim2 "index" (!) arr ix)


takeVec :: FunC Int -> Vector a -> Vector a
takeVec i (Indexed l ixf) = Indexed (minF i l) ixf

enumVec :: FunC Int -> FunC Int -> Vector (FunC Int)
enumVec f t = Indexed (t - f + 1) (\ix -> ix + f)

instance Functor Vector where
  fmap f (Indexed l ixf) = Indexed l (f . ixf)

minF :: FunC Int -> FunC Int -> FunC Int
minF a b = Prim2 "min" Prelude.min a b

(>=) :: FunC Int -> FunC Int -> FunC Bool
a >= b = Prim2 "(>=)" (Prelude.>=) a b

-- Example programs

modulus :: FunC Int -> FunC Int -> FunC Int
modulus a b = while (>=b) (subtract b) a

modulusTest = modulus 17 2

vecTest n = fmap (+7) $ fmap (*3) $ enumVec 5 n

vecTestPrint = putStrLn $ show $ vecTest

divTest :: FunC Int -> FunC Int -> FunC Int -> Option (FunC Int)
divTest a b c = do r1 <- divO a b
                   r2 <- divO a c
                   return (a+b)