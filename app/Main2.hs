
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

  data FirstTuple
  data SecondTuple

  data F a b = F b deriving Show
  data S a b = S b deriving Show

  class Tuple a where
    val :: a b -> b

  instance a ~ FirstTuple => Tuple (F a) where
    val (F v) = v

  instance a ~ SecondTuple => Tuple (S a) where
    val (S v) = v

  type First a = F FirstTuple a
  type Second a = S SecondTuple a

  type a @@ b = (First a, Second b)

  type family Fst a where
    Fst (First a, Second b) = a
    Fst (First a) = Fst a
    Fst (Second a) = Fst a

  type family Snd a where
    Snd (First a, Second b) = b
    Snd (First a) = Snd a
    Snd (Second a) = Snd a

  tfst :: tuple ~ (a @@ b) => tuple -> Fst tuple
  tfst (F a, _) = a

  tsnd :: tuple ~ (a @@ b) => tuple -> Snd tuple
  tsnd (_, S b) = b

  (@@) :: a -> b -> a @@ b
  a @@ b = (F a, S b)

  tpl1 :: Int @@ String
  tpl1 = 1 @@ "hello"

  tpl2 :: Snd (String @@ String)
  tpl2 = tsnd ("hello" @@ "world")

  tpl3 :: Snd (String @@ String)
  tpl3 = tsnd ("hello" @@ "world")

  tpl4 :: Int @@ String
  tpl4 = 1 @@ "hello"

  tpl5 :: Snd (String @@ (String @@ String))
  tpl5 = tsnd ("hello" @@ ("hello" @@ "world"))

  tpl6 :: Fst (Snd ((String @@ String) @@ (String @@ String)))
  tpl6 = tfst . tsnd $ (("world" @@ "hello") @@ ("hello" @@ "world"))

  main :: IO ()
  main = do
    print tpl4
    print tpl3
    print tpl2
