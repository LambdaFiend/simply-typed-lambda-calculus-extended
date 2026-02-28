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
  = TmVar Index Index Name
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
  | TmErr String
  | TmTyAbs Name TermNode
  | TmTyApp TermNode Type
  | TmPack Type TermNode Type
  | TmUnpack String String TermNode TermNode
  deriving (Eq, Show)

data Type
  = TyBool
  | TyNat
  | TyUnit
  | TyArr Type Type
  | TyRecord [(Name, Type)]
  | TyVariant [(Name, Type)]
  | TyList Type
  | TyUnknown
  | TyVar Int
  | TyScheme [Type] Type
  | TyErr String
  | TyForAll Name Type
  | TyForSome Name Type
  | TyVarFRaw Name
  | TyVarF Index Index Name
  deriving Show

instance Eq Type where
  TyForAll _ ty == TyForAll _ ty' = ty == ty'
  TyForSome _ ty == TyForSome _ ty' = ty == ty'
  TyVarF ind1 ind2 _ == TyVarF ind1' ind2' _ =
    ind1 == ind1' && ind2 == ind2'
  TyBool == TyBool = True
  TyNat == TyNat = True
  TyUnit == TyUnit = True
  TyArr ty1 ty2 == TyArr ty1' ty2' =
    ty1 == ty1' && ty2 == ty2'
  TyRecord xs == TyRecord ys = xs == ys
  TyVariant xs == TyVariant ys = xs == ys
  TyList ty1 == TyList ty1' = ty1 == ty1'
  TyUnknown == TyUnknown = True
  TyVar n == TyVar n' = n == n'
  TyScheme tys ty == TyScheme tys' ty' =
    tys == tys' && ty == ty'
  TyErr s == TyErr s' = s == s'
  TyVarFRaw name == TyVarFRaw name' = name == name'    
  _ == _ = False

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

