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
  | TmWildCard Type TermNode
  | TmAscribe TermNode Type
  | TmLet Name TermNode TermNode
  | TmProj TermNode Name
  | TmRecord [(Name, TermNode)]
  deriving (Eq, Show)

data Type
  = TyBool
  | TyNat
  | TyUnit
  | TyArr Type Type
  | TyRecord [(Name, Type)]
  deriving (Eq, Show)

type Context = [(Name, Type)]

noPos :: FileInfo
noPos = AlexPn (-1) (-1) (-1)

fromMaybeTm :: Maybe TermNode -> TermNode
fromMaybeTm (Just x) = x
fromMaybeTm Nothing = error "Oops??? Nothing???"

fromMaybeType :: Maybe Type -> Type
fromMaybeType (Just x) = x
fromMaybeType Nothing = error "Oops??? Nothing???"

