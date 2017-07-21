# haskellua

haskellua is a DSL for writing Lua in Haskell.

There is one central class:

```haskell
class Lua a where
  emit :: a -> Text
```

And two support classes:
```haskell
class Lua a => Stmt a
class Lua a => Expr a
```

## data types

```newtype LNumber = LNumber Integer``` for representing lua numbers

```newtype LString = LString Text``` for representing lua strings

```newtype LSymbol = LSymbol Text``` for representing symbols

```newtype LBool = LBool Text``` for representing booleans

```data LNil``` for nil

```data LFunDef = LFunDef Text [LSymbol] LBody``` for function definitions

```data LFunCall = forall a. Expr a => LFunCall Text [a]``` for function calls

```data LAssignment``` for assignments

```data LInfix``` for infix operators

```data LInitializer``` for for-loop initializers

```data LBody``` for block bodies

```data LBlock``` for blocks including control structures
