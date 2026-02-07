module Typing where

import Syntax
import Display

typeOf' :: TermNode -> Type
typeOf' t = typeOf [] t

typeOf :: Context -> TermNode -> Type
typeOf ctx t =
  case tm of
    TmVar k l -> getTypeFromContext ctx k
    TmAbs x ty t1 -> TyArr ty $ typeOf ((x, ty):ctx) t1
    TmApp t1 t2 ->
      let tyT1 = typeOf' t1; tyT2 = typeOf' t2 in
      case tyT1 of
        TyArr tyT11 tyT12 ->
          if tyT11 == tyT2
            then tyT12
            else error $ tmAppErr2 tyT11 tyT2
        _ -> error $ tmAppErr1 tyT1 
    TmTrue -> TyBool
    TmFalse -> TyBool
    TmIf t1 t2 t3 ->
      let tyT1 = typeOf' t1; tyT2 = typeOf' t2; tyT3 = typeOf' t3 in
      if tyT1 == TyBool
        then
          if tyT2 == tyT3
            then tyT2
            else error $ tmIfErr2 tyT2 tyT3
        else error $ tmIfErr1 tyT1
    TmZero -> TyNat
    TmSucc t1 -> checkNat TyNat tmSuccErr t1
    TmPred t1 -> checkNat TyNat tmPredErr t1
    TmIsZero t1 -> checkNat TyBool tmIsZeroErr t1
    TmUnit -> TyUnit
    _ -> error ("No rule applies: " ++ showFileInfo fi)

  where tm = getTm t
        fi = getFI t
        typeOf' = typeOf ctx
        checkNat :: Type -> (Type -> String) -> TermNode -> Type
        checkNat typ err t1 =
          let tyT1 = typeOf' t1 in
          if tyT1 == TyNat
            then typ
            else error $ err tyT1
        tmAppErr1 tyT1 = "TmApp: expected TyArr, but got " ++ show tyT1
        tmAppErr2 tyT11 tyT2 = "TmApp: type mismatch, where tyT11 is " ++ show tyT11 ++ " and tyT2 is " ++ show tyT2
        tmIfErr1 tyT1 = "TmIf: expected bool, but got show" ++ show tyT1
        tmIfErr2 tyT2 tyT3 = "TmIf: type mismatch, where tyT2 is " ++ show tyT2 ++ " and tyT3 is " ++ show tyT3
        tmSuccErr tyT1 = "TmSucc: expected TyNat, but got " ++ show tyT1
        tmPredErr tyT1 = "TmPred: expected TyNat, but got " ++ show tyT1
        tmIsZeroErr tyT1 = "TmIsZero: expected TyNat, but got " ++ show tyT1



getTypeFromContext :: Context -> Index -> Type
getTypeFromContext ctx ind | ind < length ctx = snd $ (ctx !! ind)
                           | otherwise = error "TmVar: no type context for variable"

