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
        TyArr tyT11 tyT12 -> check3 tyT11 tyT2 tyT12 $ tmAppErr2 tyT11 tyT2
        _ -> error $ tmAppErr1 tyT1 
    TmTrue -> TyBool
    TmFalse -> TyBool
    TmIf t1 t2 t3 ->
      let tyT1 = typeOf' t1; tyT2 = typeOf' t2; tyT3 = typeOf' t3 in
      if tyT1 == TyBool
        then check2 tyT2 tyT3 $ tmIfErr2 tyT2 tyT3
        else error $ tmIfErr1 tyT1
    TmZero -> TyNat
    TmSucc t1 -> let tyT1 = typeOf' t1 in check3 tyT1 TyNat TyNat $ tmSuccErr tyT1
    TmPred t1 -> let tyT1 = typeOf' t1 in check3 tyT1 TyNat TyNat $ tmPredErr tyT1
    TmIsZero t1 -> let tyT1 = typeOf' t1 in check3 tyT1 TyNat TyBool $ tmIsZeroErr tyT1
    TmUnit -> TyUnit
    TmAscribe t1 ty -> let tyT1 = typeOf' t1 in check2 tyT1 ty $ tmAscribeErr tyT1 ty
    TmSeq t1 t2 -> let tyT1 = typeOf' t1 in check3 tyT1 TyUnit (typeOf' t2) $ tmSeqErr tyT1
    TmWildCard ty t1 -> TyArr ty $ typeOf' t1
    TmLet x t1 t2 -> typeOf ((x, typeOf' t1):ctx) t2
    TmProj (TermNode _ (TmRecord ts)) x
      | lookup x ts /= Nothing -> fromMaybeType $ lookup x (map (\(x, y) -> (x, typeOf' y)) ts)
    TmRecord ts -> TyRecord $ map (\(x, y) -> (x, typeOf' y)) ts
    _ -> error ("No rule applies: " ++ showFileInfo fi)

  where tm = getTm t
        fi = getFI t
        typeOf' = typeOf ctx
        check3 :: Type -> Type -> Type -> String -> Type
        check3 ty1 ty2 ty3 err = if ty1 == ty2 then ty3 else error err
        check2 :: Type -> Type -> String -> Type
        check2 ty1 ty2 err = check3 ty1 ty2 ty2 err

        tmAppErr1 tyT1 = "TmApp: expected TyArr, but got " ++ showType tyT1 ++ showFileInfo fi
        tmAppErr2 tyT11 tyT2 = "TmApp: type mismatch, where tyT11 is " ++ showType tyT11 ++ " and tyT2 is " ++ showType tyT2 ++ showFileInfo fi
        tmIfErr1 tyT1 = "TmIf: expected bool, but got show" ++ showType tyT1 ++ showFileInfo fi
        tmIfErr2 tyT2 tyT3 = "TmIf: type mismatch, where tyT2 is " ++ showType tyT2 ++ " and tyT3 is " ++ showType tyT3 ++ showFileInfo fi
        tmSuccErr tyT1 = "TmSucc: expected TyNat, but got " ++ showType tyT1 ++ showFileInfo fi
        tmPredErr tyT1 = "TmPred: expected TyNat, but got " ++ showType tyT1 ++ showFileInfo fi
        tmIsZeroErr tyT1 = "TmIsZero: expected TyNat, but got " ++ showType tyT1 ++ showFileInfo fi
        tmAscribeErr tyT1 ty = "TmAscribe: expected " ++ showType ty ++ " but got " ++ showType tyT1 ++ showFileInfo fi
        tmSeqErr tyT1 = "TmSeq: expected TyUnit, but got " ++ showType tyT1 ++ showFileInfo fi

getTypeFromContext :: Context -> Index -> Type
getTypeFromContext ctx ind | ind < length ctx = snd $ (ctx !! ind)
                           | otherwise = error "TmVar: no type context for variable"

