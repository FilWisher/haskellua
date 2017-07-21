{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module Lua where

import qualified Data.Text as T
import Data.List (intersperse)

class Lua a where
  emit :: a -> T.Text

class Lua a => Expr a
class Lua a => Stmt a

newtype LNumber = LNumber Integer

newtype LString = LString T.Text

newtype LSymbol = LSymbol T.Text

data LFunDef = forall a. Stmt a => LFunDef T.Text [LSymbol] [a]

data LFunCall = forall a. Expr a => LFunCall T.Text [a]

data LAssignment = forall a. Expr a => GlobalAssignment LSymbol a
                 | forall a. Expr a => LocalAssignment  LSymbol a

data LInfix = forall a. Expr a => LInfix a T.Text a

-- TODO: implement the loop initializers
newtype LInitializer = LInitializer T.Text

instance Lua LInitializer where
  emit (LInitializer e) = e

data LBlock = forall a. Stmt a => LDo [a]
            | forall a b. (Stmt a, Expr b) => LIf b [a] LBlock
            | forall a b. (Stmt a, Expr b) => LElseIf b [a] LBlock
            | forall a. Stmt a             => LElse [a]
            | forall a b. (Stmt a, Expr b) => LWhile b [a]
            | forall a. Stmt a             => LFor LInitializer [a] 
            | LEnd

instance Lua LBlock where
  emit (LIf p body post) = mconcat
    [ "if ", emit p, " then\n"
    , "\t", emitJoined "\n" body, "\n"
    , emit post]
  emit (LElseIf p body post) = mconcat
    [ "elseif ", emit p, " then\n"
    , "\t", emitJoined "\n" body, "\n"
    , emit post]
  emit (LElse body) = mconcat
    [ "else\n"
    , "\t", emitJoined "\n" body, "\n"
    , "end" ]
  emit (LWhile p body) = mconcat
    [ "while ", emit p, " do\n"
    , "\t", emitJoined "\n" body, "\n"
    , "end" ]
  emit (LFor init body) = mconcat
    [ "for ", emit init, " do\n"
    , "\t", emitJoined "\n" body, "\n"
    , "end"
    ]
    

instance Lua LFunDef where
  emit (LFunDef name args body) = mconcat 
    [ "function ", name, "(", argList , ")\n"
    , "\t", fullBody, "\n"
    , "end"
    ]
    where argList = emitJoined ", " args
          fullBody = emitJoined "\n" body

instance Lua LSymbol where
  emit (LSymbol v) = v

instance Lua LNumber where
  emit (LNumber i) = T.pack $ show i

instance Lua LString where
  emit (LString t) = mconcat ["\"", t, "\""]

instance Lua LFunCall where
  emit (LFunCall name args) = mconcat
    [ name, "(", emitJoined ", " args, ")" ]

instance Lua LAssignment where
  emit (GlobalAssignment sym expr) = 
    mconcat [ emit sym, " = ", emit expr ]
  emit (LocalAssignment sym expr) =
    mconcat [ "local ", emit sym, " = ", emit expr ]

instance Expr LString
instance Expr LNumber
instance Expr LSymbol
instance Expr LInfix 

instance Stmt LAssignment

instance Lua LInfix where
  emit (LInfix e1 t e2) = mconcat [emit e1, " ", t, " ", emit e2]

emitJoined :: Lua a => T.Text -> [a] -> T.Text
emitJoined div frags =
  mconcat $ intersperse div $ map emit frags
