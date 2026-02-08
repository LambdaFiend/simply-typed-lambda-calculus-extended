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
    _ -> False

isPattern :: TermNode -> Bool
isPattern t = let tm = getTm t in
  case tm of
    TmVar _ _ -> True
    TmRecord ts -> and $ map (isPattern . snd) ts
    _ -> False

match :: TermNode -> TermNode -> [TermNode -> TermNode]
match (TermNode _ (TmVar _ _)) t2 = [evalSubst t2]
match (TermNode _ (TmRecord ts1)) (TermNode _ (TmRecord ts2)) =
  concat
  $ map (\(y1, y2) -> match y1 y2)
  $ zip (map snd ts1) (map snd ts2)


eval1 :: TermNode -> TermNode
eval1 t = let tm = getTm t; fi = getFI t in
  TermNode fi $
  case tm of
    TmApp (TermNode _ (TmAbs _ _ t11)) v2
      | isVal v2 -> getTm $ evalSubst v2 t11
    TmApp (TermNode _ (TmWildCard _ t11)) v2
      | isVal v2 -> getTm t11
    TmApp v1 t2 | isVal v1 ->
      let t2' = eval1 t2
       in TmApp v1 t2'
    TmApp t1 t2 ->
      let t1' = eval1 t1
       in TmApp t1' t2
    TmIf (TermNode _ TmTrue) t2 t3 -> getTm t2
    TmIf (TermNode _ TmFalse) t2 t3 -> getTm t3
    TmIf t1 t2 t3 ->
      let t1' = eval1 t1
       in TmIf t1' t2 t3
    TmSucc t1 | not $ isVal t1 -> TmSucc $ eval1 t1
    TmPred (TermNode _ TmZero) -> TmZero
    TmPred (TermNode _ (TmSucc nv1)) | isVal nv1 -> getTm nv1
    TmPred t1 -> TmPred $ eval1 t1
    TmIsZero (TermNode _ TmZero) -> TmTrue
    TmIsZero (TermNode _ (TmSucc nv1)) | isVal nv1 -> TmFalse
    TmIsZero t1 -> TmIsZero $ eval1 t1
    TmUnit -> TmUnit
    TmSeq (TermNode _ TmUnit) t2 -> getTm t2
    TmSeq t1 t2 -> TmSeq (eval1 t1) t2
    TmAscribe t1 ty | not $ isVal t1 -> TmAscribe (eval1 t1) ty
    TmAscribe v1 _ -> getTm v1
    TmLet x t1 t2 | not $ isVal t1 -> TmLet x (eval1 t1) t2
    TmLet _ v1 t2 -> getTm $ evalSubst v1 t2
    TmProj v1@(TermNode _ (TmRecord ts)) x
      | isVal v1 && lookup x ts /= Nothing -> getTm $ fromMaybeTm $ lookup x ts
    TmProj t1 x -> TmProj (eval1 t1) x
    TmRecord ts ->
      let firstHalf = takeWhile (\(_, y) -> isVal y) ts
          secondHalf = dropWhile (\(_, y) -> isVal y) ts
          newMiddle = (\ts -> case ts of (t:ts) -> [(fst t, eval1 $ snd t)]; [] -> []) secondHalf
          rest = (\ts -> case ts of (t:ts) -> ts; [] -> []) secondHalf
       in TmRecord (firstHalf ++ newMiddle ++ rest)
    _ -> error ("No rule applies: " ++ showFileInfo fi)


eval :: TermNode -> TermNode
eval t | isVal t   = t
       | otherwise = eval $ eval1 t
