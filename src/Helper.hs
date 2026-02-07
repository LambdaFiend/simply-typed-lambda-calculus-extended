module Helper where

import Syntax
import Typing

newtype UpdatedTmArrTm = UpdatedTmArrTm
  { updateTmArrTm :: (TermNode, TermNode -> UpdatedTmArrTm) }

traverseUpTm :: (TermNode -> TermNode) -> TermNode -> TermNode
traverseUpTm f t = f $ TermNode fi $
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
  where tm = getTm t
        fi = getFI t
        traverseTm' = traverseUpTm f

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
    _ -> (t, shift')


subst' :: Index -> TermNode -> TermNode -> TermNode
subst' j s t = traverseDownTm (subst 0 j s) t

subst :: Index -> Index -> TermNode -> TermNode -> UpdatedTmArrTm
subst c j s t = let tm = getTm t; subst' = subst c j s in
  UpdatedTmArrTm $
  case tm of
    TmVar k l -> (if k == j + c then shift' 0 c s else t, subst')
    TmAbs x ty t1 -> (t, subst (c + 1) j s)
    _ -> (t, subst')


genIndex' :: TermNode -> TermNode
genIndex' t = traverseDownTm (genIndex []) t

genIndex :: [Name] -> TermNode -> UpdatedTmArrTm
genIndex ctx t = let tm = getTm t; fi = getFI t; genIndex' = genIndex ctx in
  UpdatedTmArrTm $
  case tm of
    TmVarRaw x -> (TermNode fi $ TmVar (length $ takeWhile (/= x) ctx) (length ctx), genIndex')
    TmAbs x ty t1 -> (t, genIndex (x:ctx))
    _ -> (t, genIndex')


desugarTm' :: TermNode -> TermNode
desugarTm' t = traverseUpTm desugarTm t

desugarTm :: TermNode -> TermNode
desugarTm (TermNode fi (TmSeq t1 t2)) = let ty = typeOf' t1 in
  TermNode fi $ TmApp (TermNode fi $ TmAbs "x" ty (shift' 0 1 t2)) t1
desugarTm t = t
