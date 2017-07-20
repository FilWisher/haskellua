{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Lib
    ( someFunc
    , LNumber(..)
    , Lua(..)
    , test1
    , test2
    , runTest
    ) where

import qualified Data.Text as T
import Data.Text.IO as IO
import Data.List (intersperse)

class Lua a where
  emit :: a -> T.Text

newtype LNumber = LNumber Integer

instance Lua LNumber where
  emit (LNumber i) = T.pack $ show i

newtype LString = LString T.Text

instance Lua LString where
  emit (LString t) = mconcat ["\"", t, "\""]

newtype LSymbol = LSymbol T.Text

instance Lua LSymbol where
  emit (LSymbol v) = v

data LFunDef = LFunDef T.Text [LSymbol] [LStatement]

instance Lua LFunDef where
  emit (LFunDef name args body) = mconcat 
    [ "function ", name, "(", argList , ")\n"
    , "\t", fullBody, "\n"
    , "end"
    ]
    where argList = emitJoined ", " args
          fullBody = emitJoined "\n" body

data LFunCall = LFunCall T.Text [LExpr]

emitJoined :: Lua a => T.Text -> [a] -> T.Text
emitJoined div frags =
  mconcat $ intersperse div $ map emit frags

instance Lua LFunCall where
  emit (LFunCall name args) = mconcat
    [ name, "(", emitJoined ", " args, ")" ]

data LStatement = GlobalAssignment LSymbol LExpr
                | LocalAssignment  LSymbol LExpr

instance Lua LStatement where
  emit (GlobalAssignment sym expr) = 
    mconcat [ emit sym, " = ", emit expr ]
  emit (LocalAssignment sym expr) =
    mconcat [ "local ", emit sym, " = ", emit expr ]

-- TODO: Needs completed list of expressions
-- This is so that we can selectively classify
-- some of our datatypes as expressions and
-- constrain some of the data constructor params.
-- 
-- A possible alternative is to declare a class
-- Expr which these datatypes can inhabit. This
-- would mean we would need GADTs to express the
-- constraints in the data constructors but would
-- mean less unwrapping of data constructors.
data LExpr = EString LString
              | ENumber LNumber
              | EInfix LExpr T.Text LExpr
              | ESymbol LSymbol

instance Lua LExpr where
  emit (EString e) = emit e
  emit (ENumber e) = emit e
  emit (EInfix e1 t e2) = mconcat [emit e1, " ", t, " ", emit e2]
  emit (ESymbol e) = emit e

test1 = LFunDef "hello" [LSymbol "a", LSymbol "b"]
  [ GlobalAssignment (LSymbol "global") (EInfix (ENumber (LNumber 2)) "+" (ENumber (LNumber 1))) ]

test2 = LFunCall "print" [EString (LString "hello"), EString (LString "there")]

runTest :: Lua a => a -> IO ()
runTest = IO.putStrLn . emit

someFunc :: IO ()
someFunc = IO.putStrLn $ emit test1
