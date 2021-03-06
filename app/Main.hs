{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

  data Fst
  data Snd

  data F a b = F b deriving Show
  data S a b = S b deriving Show

  class TupleVal a where
    val :: a b -> b

  instance a ~ Fst => TupleVal (F a) where
    val (F v) = v

  instance a ~ Fst => TupleVal (S a) where
    val (S v) = v

  class TupleOps (a :: * -> * -> *) b c where
    type FST a b c
    type SND a b c
    tfst :: a b c -> FST a b c
    tsnd :: a b c -> SND a b c

  instance (b ~ F b' b'', b' ~ Fst, c ~ S c' c'', c' ~ Snd) => TupleOps (,) b c where
    type FST (,) b c = b
    type SND (,) b c = c
    tfst = fst
    tsnd = snd

  instance TupleOps F b (c1, c2) where
    type FST F b (c1, c2) = c1
    type SND F b (c1, c2) = c2
    tfst (F (v1, v2)) = v1
    tsnd (F (v1, v2)) = v2

  instance TupleOps S b (c1, c2) where
    type FST S b (c1, c2) = c1
    type SND S b (c1, c2) = c2
    tfst (S (v1, v2)) = v1
    tsnd (S (v1, v2)) = v2

  type First a = F Fst a
  type Second a = S Snd a

  type a @@ b = (First a, Second b)

  type family a :=: b where
    Fst :=: b = Fst' b
    Snd :=: b = Snd' b

  infixr 1 :=:

  type family Fst' a where
    Fst' (a, b) = a
    Fst' (First a) = Fst' a
    Fst' (Second a) = Fst' a

  type family Snd' a where
    Snd' (a, b) = b
    Snd' (First a) = Snd' a
    Snd' (Second a) = Snd' a

  (@@) :: a -> b -> a @@ b
  a @@ b = (F a, S b)

  tpl1 :: Int @@ String
  tpl1 = 1 @@ "hello"

  tpl2 :: Snd :=: String @@ String
  tpl2 = tsnd ("hello" @@ "world")

  tpl3 :: Snd :=: String @@ String
  tpl3 = tsnd ("hello" @@ "world")

  tpl4 :: Int @@ String
  tpl4 = 1 @@ "hello"

  tpl5 :: Snd :=: String @@ (String @@ String)
  tpl5 = tsnd ("hello" @@ ("hello" @@ "world"))

  tpl6 :: Snd :=: Fst :=: Snd :=: (String @@ String) @@ ((String @@ String) @@ String)
  tpl6 = tsnd . tfst . tsnd $ ("world1" @@ "hello2") @@ (("hello3" @@ "world5") @@ "world4")

  main :: IO ()
  main = do
    print tpl4
    print tpl3
    print tpl2
    print tpl6
