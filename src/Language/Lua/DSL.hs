{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module Language.Lua.DSL where

import qualified Data.Text as T
import Data.List (intersperse)

class Lua a where
  emit :: a -> T.Text

class Lua a => Expr a
class Lua a => Stmt a

newtype LNumber = LNumber Integer

newtype LString = LString T.Text

newtype LSymbol = LSymbol T.Text

newtype LBool = LBool Bool

data LNil 

data LFunDef = LFunDef T.Text [LSymbol] LBody

data LFunCall = forall a. Expr a => LFunCall T.Text [a]

data LAssignment = forall a. Expr a => GlobalAssignment LSymbol a
                 | forall a. Expr a => LocalAssignment  LSymbol a

data LInfix = forall a. Expr a => LInfix a T.Text a

-- TODO: implement the loop initializers
newtype LInitializer = LInitializer T.Text

instance Lua LInitializer where
  emit (LInitializer e) = e

data LBody = forall a. Stmt a => LBody [a]

data LBlock = LDo LBody
            | forall a. Expr a => LIf a LBody LBlock
            | forall a. Expr a => LElseIf a LBody LBlock
            | LElse LBody
            | forall a. Expr a => LWhile a LBody
            | LFor LInitializer LBody 
            | LEnd

instance Lua LBody where
  emit (LBody body) = mconcat $ intersperse "\n" $ map emit body

instance Lua LBlock where
  emit (LIf p body post) = mconcat
    [ "if ", emit p, " then\n"
    , "\t", emit body, "\n"
    , emit post]
  emit (LElseIf p body post) = mconcat
    [ "elseif ", emit p, " then\n"
    , "\t", emit body, "\n"
    , emit post]
  emit (LElse body) = mconcat
    [ "else\n"
    , "\t", emit body, "\n"
    , "end" ]
  emit (LWhile p body) = mconcat
    [ "while ", emit p, " do\n"
    , "\t", emit body, "\n"
    , "end" ]
  emit (LFor init body) = mconcat
    [ "for ", emit init, " do\n"
    , "\t", emit body, "\n"
    , "end"
    ]
    
instance Lua LFunDef where
  emit (LFunDef name args body) = mconcat 
    [ "function ", name, "(", argList , ")\n"
    , "\t", emit body, "\n"
    , "end"
    ]
    where argList = mconcat $ intersperse "," $ map emit args

instance Lua LFunCall where
  emit (LFunCall name args) = mconcat
    [ name, "(", argList, ")" ]
    where argList = mconcat $ intersperse "," $ map emit args

instance Lua LSymbol where
  emit (LSymbol v) = v

instance Lua LNumber where
  emit (LNumber i) = T.pack $ show i

instance Lua LString where
  emit (LString t) = mconcat ["\"", t, "\""]

instance Lua LNil where
  emit _ = "nil"

instance Lua LBool where
  emit (LBool bool) = T.pack $ show bool

instance Lua LAssignment where
  emit (GlobalAssignment sym expr) = 
    mconcat [ emit sym, " = ", emit expr ]
  emit (LocalAssignment sym expr) =
    mconcat [ "local ", emit sym, " = ", emit expr ]

instance Lua LInfix where
  emit (LInfix e1 t e2) = mconcat [emit e1, " ", t, " ", emit e2]

instance Expr LString
instance Expr LNumber
instance Expr LSymbol
instance Expr LInfix 
instance Expr LBool 
instance Expr LNil

instance Stmt LAssignment
instance Stmt LBlock
instance Stmt LBody
