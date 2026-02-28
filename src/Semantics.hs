module Semantics where

import Syntax
import Helper
import Display

match :: Pattern -> TermNode -> [TermNode -> TermNode]
match (PVar _) t2 = [evalSubst t2]
match (PRecord ps1) (TermNode _ (TmRecord ts2))
  | length ps1 == length ts2 && (and $ map (\((x1, _), (x2, _)) -> x1 == x2) $ zip ps1 ts2) =
    concat
      $ map (\(y1, y2) -> match y1 y2)
      $ zip (map snd ps1) (map snd ts2)
match x y = [(\x -> TermNode noPos $ TmErr "Matching error")]

eval1 :: TermNode -> TermNode
eval1 t = let tm = getTm t; fi = getFI t in
  TermNode fi $
  case tm of
    TmApp (TermNode _ (TmAbs _ _ t11)) v2 | isVal v2 -> getTm $ evalSubst v2 t11
    TmApp (TermNode _ (TmWildCard _ t11)) v2 | isVal v2 -> getTm t11
    TmApp v1 t2 | isVal v1 -> let result = eval1 t2 in
      case getTm result of
        TmErr e -> TmErr e
        _ -> TmApp v1 result
    TmApp t1 t2 | not $ isVal t1 -> let result = eval1 t1 in
      case getTm result of
        TmErr e -> TmErr e
        _ -> TmApp result t2
    TmIf (TermNode _ TmTrue) t2 t3 -> getTm t2
    TmIf (TermNode _ TmFalse) t2 t3 -> getTm t3
    TmIf t1 t2 t3 | not $ isVal t1 -> let result = eval1 t1 in
      case getTm result of
        TmErr e -> TmErr e
        _ -> TmIf result t2 t3
    TmSucc t1 | not $ isVal t1 -> let result = eval1 t1 in
      case getTm result of
        TmErr e -> TmErr e
        _ -> TmSucc result
    TmPred (TermNode _ TmZero) -> TmZero
    TmPred (TermNode _ (TmSucc nv1)) | isVal nv1 -> getTm nv1
    TmPred t1 | not $ isVal t1 -> let result = eval1 t1 in
      case getTm result of
        TmErr e -> TmErr e
        _ -> TmPred result
    TmIsZero (TermNode _ TmZero) -> TmTrue
    TmIsZero (TermNode _ (TmSucc nv1)) | isVal nv1 -> TmFalse
    TmIsZero t1 | not $ isVal t1 -> let result = eval1 t1 in
      case getTm result of
        TmErr e -> TmErr e
        _ -> TmIsZero result
    TmUnit -> TmUnit
    TmSeq (TermNode _ TmUnit) t2 -> getTm t2
    TmSeq t1 t2 | not $ isVal t1 -> let result = eval1 t1 in
      case getTm result of
        TmErr e -> TmErr e
        _ -> TmSeq result t2
    TmAscribe t1 ty | not $ isVal t1 -> let result = eval1 t1 in
      case getTm result of
        TmErr e -> TmErr e
        _ -> TmAscribe result ty
    TmAscribe v1 _ -> getTm v1
    TmLet p t1 t2 | not $ isVal t1 -> let result = eval1 t1 in
      case getTm result of
        TmErr e -> TmErr e
        _ -> TmLet p result t2
    TmLet p v1 t2 -> let subs = reverse $ match p v1; result = foldr ($) t2 subs in
      if elem (TermNode noPos $ TmErr "Matching error") $ map (\x -> x $ TermNode noPos TmTrue) subs
        then TmErr "Matching error, either not perfect matching slots or non-matching labels"
        else getTm result
    TmProj v1@(TermNode _ (TmRecord ts)) x | isVal v1 -> getTm $ fromMaybe $ lookup x ts
    TmProj t1 x -> let result = eval1 t1 in
      case getTm result of
        TmErr e -> TmErr e
        _ -> TmProj result x
    TmRecord ts ->
      let firstHalf = takeWhile (\(_, y) -> isVal y) ts
          secondHalf = dropWhile (\(_, y) -> isVal y) ts
          newMiddle = (\ts -> case ts of (t:ts) -> [(fst t, eval1 $ snd t)]; [] -> []) secondHalf
          rest = (\ts -> case ts of (t:ts) -> ts; [] -> []) secondHalf
       in
        case newMiddle of
          [(_, TermNode _ (TmErr e))] -> TmErr e
          _ -> TmRecord (firstHalf ++ newMiddle ++ rest)
    TmVariant x t1 ty | not $ isVal t1 -> let result = eval1 t1 in
      case getTm result of
        TmErr e -> TmErr e
        _ -> TmVariant x result ty
    TmCase t1 ts1 | not $ isVal t1 -> let result = eval1 t1 in
      case getTm result of
        TmErr e -> TmErr e
        _ -> TmCase result ts1
    TmCase (TermNode _ (TmVariant x v1 ty)) ts -> getTm $ evalSubst v1 $ snd $ fromMaybe $ lookup x ts
    TmFix (TermNode _ (TmAbs x ty t2)) -> getTm $  evalSubst t t2
    TmFix t1 | not $ isVal t1 -> let result = eval1 t1 in
      case getTm result of
        TmErr e -> TmErr e
        _ -> TmFix result
    TmCons ty v1 t2 | isVal v1 -> let result = eval1 t2 in
      case getTm result of
        TmErr e -> TmErr e
        _ -> TmCons ty v1 result
    TmCons ty t1 t2 -> let result = eval1 t1 in
      case getTm result of
        TmErr e -> TmErr e
        _ -> TmCons ty result t2
    TmIsNil _ (TermNode _ (TmNil _)) -> TmTrue
    TmIsNil _ (TermNode _ (TmCons _ v1 v2)) | isVal v1 && isVal v2 -> TmFalse
    TmIsNil ty t1 | not $ isVal t1 -> let result = eval1 t1 in
      case getTm result of
        TmErr e -> TmErr e
        _ -> TmIsNil ty $ eval1 t1
    TmHead _ (TermNode _ (TmCons _ v1 v2)) | isVal v1 && isVal v2 -> getTm v1
    TmHead ty t1 | not $ isVal t1 -> let result = eval1 t1 in
      case getTm result of
        TmErr e -> TmErr e
        _ -> TmHead ty result
    TmTail _ (TermNode _ (TmCons _ v1 v2)) | isVal v1 && isVal v2 -> getTm v2
    TmTail ty t1 | not $ isVal t1 -> let result = eval1 t1 in
      case getTm result of
        TmErr e -> TmErr e
        _ -> TmTail ty result
    TmTyApp t1 ty | not $ isVal t1 -> let result = eval1 t1 in
      case getTm result of
        TmErr e -> TmErr e
        _ -> TmTyApp result ty
    TmTyApp (TermNode _ (TmTyAbs _ t11)) ty -> getTm $ evalTyTermSubst ty t11
    TmUnpack _ _ (TermNode _ (TmPack ty11 v11 ty12)) t2 | isVal v11 ->
      getTm $ evalTyTermSubst ty11 (evalSubst (shift' 0 1 v11) t2)
    TmUnpack x1 x2 t1 t2 | not $ isVal t1 -> let result = eval1 t1 in
      case getTm result of
        TmErr e -> TmErr e
        _ -> TmUnpack x1 x2 result t2
    TmPack ty1 t1 ty2 | not $ isVal t1 -> let result = eval1 t1 in
      case getTm result of
        TmErr e -> TmErr e
        _ -> TmPack ty1 result ty2
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
