# haskellua

haskellua is a DSL for writing Lua in Haskell.

There is one central class:

```haskell
class Lua a where
  emit :: a -> Text
```

And two support classes:
```haskell
class Stmt a
class Expr a
```
