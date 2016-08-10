# typed-tuples
Typed tuples sample

```Haskell
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
```
