module Syntax where

import Lexer

type Name = String
type Index = Int
type FileInfo = AlexPosn

data TermNode = TermNode
  { getFI :: FileInfo
  , getTm :: Term
  }
  deriving (Eq, Show)

data Term
  = TmVar Index Index
  | TmVarRaw Name
  | TmAbs Name Type TermNode
  | TmApp TermNode TermNode
  | TmTrue
  | TmFalse
  | TmIf TermNode TermNode TermNode
  | TmZero
  | TmSucc TermNode
  | TmPred TermNode
  | TmIsZero TermNode
  | TmUnit
  | TmSeq TermNode TermNode
  deriving (Eq, Show)

data Type
  = TyBool
  | TyNat
  | TyUnit
  | TyArr Type Type
  deriving (Eq, Show)

type Context = [(Name, Type)]

noPos :: FileInfo
noPos = AlexPn (-1) (-1) (-1)

