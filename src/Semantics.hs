module Semantics where

import Syntax
import Helper
import Display

evalSubst :: TermNode -> TermNode -> TermNode
evalSubst s t = shift' 0 (-1) (subst' 0 (shift' 0 1 s) t)

isVal :: TermNode -> Bool
isVal t = let tm = getTm t in
  case tm of
    TmAbs _ _ _ -> True
    TmUnit -> True
    TmTrue -> True
    TmFalse -> True
    TmZero -> True
    TmSucc nv | isVal nv -> True
    TmRecord ts -> and $ map (isVal . snd) ts
    TmVariant x v1 ty | isVal v1 -> True
    TmWildCard ty t11 -> True
    TmNil ty -> True
    TmCons ty v1 v2 | isVal v1 && isVal v2 -> True
    _ -> False

isPattern :: TermNode -> Bool
isPattern t = let tm = getTm t in
  case tm of
    TmVar _ _ _ -> True
    TmRecord ts -> and $ map (isPattern . snd) ts
    _ -> False

match :: Pattern -> TermNode -> [TermNode -> TermNode]
match (PVar _) t2 = [evalSubst t2]
match (PRecord ps1) (TermNode _ (TmRecord ts2)) =
  concat
  $ map (\(y1, y2) -> match y1 y2)
  $ zip (map snd ps1) (map snd ts2)

eval1 :: TermNode -> TermNode
eval1 t = let tm = getTm t; fi = getFI t in
  TermNode fi $
  case tm of
    TmApp (TermNode _ (TmAbs _ _ t11)) v2 | isVal v2 -> getTm $ evalSubst v2 t11
    TmApp (TermNode _ (TmWildCard _ t11)) v2 | isVal v2 -> getTm t11
    TmApp v1 t2 | isVal v1 -> TmApp v1 (eval1 t2)
    TmApp t1 t2 | not $ isVal t1 -> TmApp (eval1 t1) t2
    TmIf (TermNode _ TmTrue) t2 t3 -> getTm t2
    TmIf (TermNode _ TmFalse) t2 t3 -> getTm t3
    TmIf t1 t2 t3 | not $ isVal t1 -> TmIf (eval1 t1) t2 t3
    TmSucc t1 | not $ isVal t1 -> TmSucc $ eval1 t1
    TmPred (TermNode _ TmZero) -> TmZero
    TmPred (TermNode _ (TmSucc nv1)) | isVal nv1 -> getTm nv1
    TmPred t1 | not $ isVal t1 -> TmPred $ eval1 t1
    TmIsZero (TermNode _ TmZero) -> TmTrue
    TmIsZero (TermNode _ (TmSucc nv1)) | isVal nv1 -> TmFalse
    TmIsZero t1 | not $ isVal t1 -> TmIsZero $ eval1 t1
    TmUnit -> TmUnit
    TmSeq (TermNode _ TmUnit) t2 -> getTm t2
    TmSeq t1 t2 | not $ isVal t1 -> TmSeq (eval1 t1) t2
    TmAscribe t1 ty | not $ isVal t1 -> TmAscribe (eval1 t1) ty
    TmAscribe v1 _ -> getTm v1
    TmLet p t1 t2 | not $ isVal t1 -> TmLet p (eval1 t1) t2
    TmLet p v1 t2 -> getTm $ foldr ($) t2 (match p v1)
    TmProj v1@(TermNode _ (TmRecord ts)) x | isVal v1 -> getTm $ fromMaybe $ lookup x ts
    TmProj t1 x -> TmProj (eval1 t1) x
    TmRecord ts ->
      let firstHalf = takeWhile (\(_, y) -> isVal y) ts
          secondHalf = dropWhile (\(_, y) -> isVal y) ts
          newMiddle = (\ts -> case ts of (t:ts) -> [(fst t, eval1 $ snd t)]; [] -> []) secondHalf
          rest = (\ts -> case ts of (t:ts) -> ts; [] -> []) secondHalf
       in TmRecord (firstHalf ++ newMiddle ++ rest)
    TmVariant x t1 ty | not $ isVal t1 -> TmVariant x (eval1 t1) ty
    TmCase t1 ts1 | not $ isVal t1 -> TmCase (eval1 t1) ts1
    TmCase (TermNode _ (TmVariant x v1 ty)) ts -> getTm $ evalSubst v1 $ snd $ fromMaybe $ lookup x ts
    TmFix (TermNode _ (TmAbs x ty t2)) -> getTm $  evalSubst t t2
    TmFix t1 | not $ isVal t1 -> TmFix $ eval1 t1
    TmCons ty v1 t2 | isVal v1 -> TmCons ty v1 (eval1 t2)
    TmCons ty t1 t2 -> TmCons ty (eval1 t1) t2
    TmIsNil _ (TermNode _ (TmNil _)) -> TmTrue
    TmIsNil _ (TermNode _ (TmCons _ v1 v2)) | isVal v1 && isVal v2 -> TmFalse
    TmIsNil ty t1 | not $ isVal t1 -> TmIsNil ty $ eval1 t1
    TmHead _ (TermNode _ (TmCons _ v1 v2)) | isVal v1 && isVal v2 -> getTm v1
    TmHead ty t1 | not $ isVal t1 -> TmHead ty $ eval1 t1
    TmTail _ (TermNode _ (TmCons _ v1 v2)) | isVal v1 && isVal v2 -> getTm v2
    TmTail ty t1 | not $ isVal t1 -> TmTail ty $ eval1 t1 
    _ -> TmErr $ "No rule applies" ++ showFileInfo fi

type Counter = Int

eval' :: TermNode -> (Counter, TermNode)
eval' t = eval 0 t

eval :: Counter -> TermNode -> (Counter, TermNode)
eval n t@(TermNode _ (TmErr e)) = (n, t)
eval n t | isVal t   = (n, t)
         | otherwise = eval (n + 1) $ eval1 t

evalN :: Counter -> TermNode -> (Counter, TermNode)
evalN n t@(TermNode _ (TmErr e)) = (n, t)
evalN n t | n <= 0 || isVal t = (n, t)
          | otherwise         = evalN (n - 1) $ eval1 t
