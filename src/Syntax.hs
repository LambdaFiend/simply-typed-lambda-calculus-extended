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
  | TmLet Pattern TermNode TermNode
  | TmProj TermNode Name
  | TmRecord [(Name, TermNode)]
  | TmVariant Name TermNode Type
  | TmCase TermNode [(Name, (Name, TermNode))]
  | TmFix TermNode
  | TmNil Type
  | TmCons Type TermNode TermNode
  | TmIsNil Type TermNode
  | TmHead Type TermNode
  | TmTail Type TermNode
  deriving (Eq, Show)

data Type
  = TyBool
  | TyNat
  | TyUnit
  | TyArr Type Type
  | TyRecord [(Name, Type)]
  | TyVariant [(Name, Type)]
  | TyList Type
  | TyVar Int
  | TyUnknown
  deriving (Eq, Show)

data Pattern
  = PVar Name
  | PRecord [(Name, Pattern)]
  deriving (Eq, Show)

type Context = [(Name, Type)]

noPos :: FileInfo
noPos = AlexPn (-1) (-1) (-1)

fromMaybe :: Maybe a -> a
fromMaybe (Just x) = x
fromMaybe Nothing = error "Oops??? Nothing???"

typeOfPattern :: Pattern -> Type -> [(Name, Type)]
typeOfPattern p ty =
  case (p, ty) of
    (PVar x, ty) -> [(x, ty)]
    (PRecord ps, TyRecord tys) | length ps == length tys ->
      concat $ map (\(x, y) -> typeOfPattern x y) $ zip (map snd ps) (map snd tys)
    _ -> error "No rule applies"

namesOfPattern :: Pattern -> [Name]
namesOfPattern p =
  case p of
    PVar x -> [x]
    PRecord ps -> concat $ map (namesOfPattern . snd) ps
