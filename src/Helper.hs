module Helper where

import Syntax
import Typing

newtype UpdatedTmArrTm = UpdatedTmArrTm
  { updateTmArrTm :: (TermNode, TermNode -> UpdatedTmArrTm) }

traverseDownTm :: (TermNode -> UpdatedTmArrTm) -> TermNode -> TermNode
traverseDownTm f t = TermNode fi $
  case tm of
    TmVar k l -> tm
    TmVarRaw x -> tm
    TmAbs x ty t1 -> TmAbs x ty (traverseTm' t1)
    TmApp t1 t2 -> TmApp (traverseTm' t1) (traverseTm' t2)
    TmTrue -> tm
    TmFalse -> tm
    TmIf t1 t2 t3 -> TmIf (traverseTm' t1) (traverseTm' t2) (traverseTm' t3)
    TmZero -> tm
    TmSucc t1 -> TmSucc $ traverseTm' t1
    TmPred t1 -> TmPred $ traverseTm' t1
    TmIsZero t1 -> TmIsZero $ traverseTm' t1
    TmUnit -> tm
    TmSeq t1 t2 -> TmSeq (traverseTm' t1) (traverseTm' t2)
    TmWildCard ty t2 -> TmWildCard ty $ traverseTm' t2
    TmAscribe t1 ty -> TmAscribe (traverseTm' t1) ty
    TmLet x t1 t2 -> TmLet x (traverseTm' t1) (traverseTm' t2)
    TmRecord ts -> TmRecord $ map (\(x, y) -> (x, traverseTm' y)) ts
    TmProj t1 x -> TmProj (traverseTm' t1) x
  where tm = getTm t'
        fi = getFI t'
        traverseTm' = traverseDownTm f'
        (t', f') = updateTmArrTm $ f t

shift' :: Index -> Index -> TermNode -> TermNode
shift' c d t = traverseDownTm (shift c d) t

shift :: Index -> Index -> TermNode -> UpdatedTmArrTm
shift c d t = let tm = getTm t; fi = getFI t; shift' = shift c d in
  UpdatedTmArrTm $
  case tm of
    TmVar k l -> (TermNode fi $ TmVar (if k < c then k else k + d) (l + d), shift')
    TmAbs x ty t1 -> (t, shift (c + 1) d)
    TmLet x t1 t2 -> (t, shift (c + 1) d)
    _ -> (t, shift')

subst' :: Index -> TermNode -> TermNode -> TermNode
subst' j s t = traverseDownTm (subst 0 j s) t

subst :: Index -> Index -> TermNode -> TermNode -> UpdatedTmArrTm
subst c j s t = let tm = getTm t; subst' = subst c j s in
  UpdatedTmArrTm $
  case tm of
    TmVar k l -> (if k == j + c then shift' 0 c s else t, subst')
    TmAbs x ty t1 -> (t, subst (c + 1) j s)
    TmLet x t1 t2 -> (t, subst (c + 1) j s)
    _ -> (t, subst')

genIndex' :: TermNode -> TermNode
genIndex' t = traverseDownTm (genIndex []) t

genIndex :: [Name] -> TermNode -> UpdatedTmArrTm
genIndex ctx t = let tm = getTm t; fi = getFI t; genIndex' = genIndex ctx in
  UpdatedTmArrTm $
  case tm of
    TmVarRaw x -> (TermNode fi $ TmVar (length $ takeWhile (/= x) ctx) (length ctx), genIndex')
    TmAbs x ty t1 -> (t, genIndex (x:ctx))
    TmWildCard ty t2 -> (t, genIndex ("":ctx))
    TmRecord ts -> (TermNode fi $ TmRecord (map (\((x, y), k) -> (case x of "" -> show k; _ -> x, y)) $ zip ts [1..]), genIndex')
    _ -> (t, genIndex')


desugarTm' :: TermNode -> TermNode
desugarTm' t = traverseDownTm (desugarTm 0) t

desugarTm :: Index -> TermNode -> UpdatedTmArrTm
desugarTm c t = let tm = getTm t; fi = getFI t; desugarTm' = desugarTm c in
  UpdatedTmArrTm $
  case tm of
    TmVar l k -> (TermNode fi $ TmVar (l + (c - k)) c, desugarTm')
    TmAbs x ty t1 -> (t, desugarTm (c + 1))
    TmSeq t1 t2 -> (TermNode fi $ TmApp (TermNode fi $ TmAbs "x" TyUnit t2) t1, desugarTm')
    TmWildCard ty t2 -> (TermNode fi $ TmAbs "x" ty t2, desugarTm')
    TmAscribe t1 ty -> (TermNode fi $ TmApp (TermNode fi $ TmAbs "x" ty (TermNode fi $ TmVar 0 (c + 1))) t1, desugarTm')
    TmLet x t1 t2 -> (TermNode fi $ TmApp (TermNode fi $ TmAbs x (typeOf' t1) t2) t1, desugarTm')
    _ -> (t, desugarTm')

