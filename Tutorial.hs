{-# LANGUAGE GADTs #-}
module Tutorial where

data FunC a where
  LitI  :: Int  -> FunC Int
  LitB  :: Bool -> FunC Bool
  Arr   :: FunC Int -> (FunC Int -> a) -> FunC (Array Int a)
  While :: (s -> FunC Bool) -> (s -> s) -> s -> FunC s
  If    :: FunC Bool -> FunC a -> FunC a -> FunC a
  Pair  :: FunC a -> FunC b -> FunC (a,b)
  Prim  :: String -> a -> FunC a

class Syntactic a where
  type Internal a
  toFunC   :: a -> FunC (Internal a)
  fromFunC :: FunC (Internal a) -> a

-- Shallow Embeddings

-- Complex Numbers

data Complex = Complex (FunC Float) (FunC Float)

instance Syntactic Complex where
  type Internal (Complex) = (Float,Float)
  fromFunC (Complex i r) = Pair (i,r)
  toFunC p = Complex (fst p) (snd p)

-- Vectors

data Vector a where
  Indexed :: FunC Int -> (FunC Int -> a) -> Vector a

instance Syntactic (Vector a) where
  type Internal (Vector a) = Array Int a
  fromFunC (Vector l ix) = Arr l ix
  toFunC arr = Vector (length arr) (\ix -> arr ! ix)

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
