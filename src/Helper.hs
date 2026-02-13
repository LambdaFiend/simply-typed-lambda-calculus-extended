module Helper where

import Syntax
import Typing

import Data.List

newtype UpdatedTmArrTm = UpdatedTmArrTm
  { updateTmArrTm :: (TermNode, TermNode -> UpdatedTmArrTm, TermNode -> UpdatedTmArrTm) }

traverseDownTm :: (TermNode -> UpdatedTmArrTm) -> TermNode -> TermNode
traverseDownTm f t = TermNode fi $
  case tm of
    TmVar k l x -> tm
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
    TmLet p t1 t2 -> TmLet p (traverseTm'' t1) (traverseTm' t2)
    TmRecord ts -> TmRecord $ map (\(x, y) -> (x, traverseTm' y)) ts
    TmProj t1 x -> TmProj (traverseTm' t1) x
    TmVariant x t1 ty -> TmVariant x (traverseTm' t1) ty
    TmCase t1 ts -> TmCase (traverseTm'' t1) $ map (\(x, (y, z)) -> (x, (y, traverseTm' z))) ts
    TmFix t1 -> TmFix $ traverseTm' t1
    TmNil ty -> tm
    TmCons ty t1 t2 -> TmCons ty (traverseTm' t1) (traverseTm' t2)
    TmIsNil ty t1 -> TmIsNil ty $ traverseTm' t1
    TmHead ty t1 -> TmHead ty $ traverseTm' t1
    TmTail ty t1 -> TmTail ty $ traverseTm' t1
  where tm = getTm t'
        fi = getFI t'
        traverseTm'  = traverseDownTm f'
        traverseTm'' = traverseDownTm f''
        (t', f', f'') = updateTmArrTm $ f t

shift' :: Index -> Index -> TermNode -> TermNode
shift' c d t = traverseDownTm (shift c d) t

shift :: Index -> Index -> TermNode -> UpdatedTmArrTm
shift c d t = let tm = getTm t; fi = getFI t; shift' = shift c d in
  UpdatedTmArrTm $
  case tm of
    TmVar k l x -> (TermNode fi $ TmVar (if k < c then k else k + d) (l + d) x, id', id')
    TmAbs x ty t1 -> (t, shift (c + 1) d, shift')
    TmLet p t1 t2 -> (t, shift (c + (length $ namesOfPattern p)) d, shift')
    TmCase t1 ts -> (t, shift (c + 1) d, shift')
    _ -> (t, shift', shift')

subst' :: Index -> TermNode -> TermNode -> TermNode
subst' j s t = traverseDownTm (subst 0 j s) t

subst :: Index -> Index -> TermNode -> TermNode -> UpdatedTmArrTm
subst c j s t = let tm = getTm t; subst' = subst c j s in
  UpdatedTmArrTm $
  case tm of
    TmVar k l x -> (if k == j + c then shift' 0 c s else t, id', id')
    TmAbs x ty t1 -> (t, subst (c + 1) j s, subst')
    TmLet p t1 t2 -> (t, subst (c + (length $ namesOfPattern p)) j s, subst')
    TmCase t1 ts -> (t, subst (c + 1) j s, subst')
    _ -> (t, subst', subst')

genIndex' :: TermNode -> TermNode
genIndex' t = traverseDownTm (genIndex []) t

genIndex :: [Name] -> TermNode -> UpdatedTmArrTm
genIndex ctx t = let tm = getTm t; fi = getFI t; genIndex' = genIndex ctx in
  UpdatedTmArrTm $
  case tm of
    TmVarRaw x -> (TermNode fi $ TmVar (length $ takeWhile (/= x) ctx) (length ctx) x, genIndex', genIndex')
    TmAbs x ty t1 -> (TermNode fi $ TmAbs x (genIndexType ty) t1, genIndex (x:ctx), genIndex')
    TmWildCard ty t2 -> (TermNode fi $ TmWildCard (genIndexType ty) t2, genIndex ("":ctx), genIndex')
    TmRecord ts -> (TermNode fi $ TmRecord (map (\((x, y), k) -> (case x of "" -> show k; _ -> x, y)) $ zip ts [1..]), genIndex', genIndex')
    TmLet p@(PRecord ps) t1 t2 ->
      ( TermNode fi
        $ TmLet (PRecord (map (\((x, y), k) -> (case x of "" -> show k; _ -> x, y)) $ zip ps [1..])) t1 t2
      , genIndex (namesOfPattern p ++ ctx)
      , genIndex'
      )
    TmLet p t1 t2 -> (t, genIndex (namesOfPattern p ++ ctx), genIndex')
    TmCase t1 ts -> (TermNode fi $ TmCase (traverseDownTm (genIndex ctx) t1) (map (\(x, (y, z)) -> (x, (y, traverseDownTm (genIndex (y:ctx)) z )) ) ts), id', id')
    TmVariant x t1 ty -> (TermNode fi $ TmVariant x t1 (genIndexType ty), genIndex', genIndex')
    _ -> (t, genIndex', genIndex')
  where genIndexType :: Type -> Type
        genIndexType ty =
          case ty of
            (TyRecord tys) -> TyRecord $ map (\((x, y), k) -> (case x of "" -> show k; _ -> x, y)) $ zip tys [1..]
            (TyVariant tys) -> TyVariant $ map (\(x, y) -> (x, genIndexType y)) tys
            (TyArr t1 t2) -> TyArr (genIndexType t1) (genIndexType t2)
            _ -> ty

id' :: TermNode -> UpdatedTmArrTm
id' t = UpdatedTmArrTm (t, id', id')

desugarTm' :: TermNode -> TermNode
desugarTm' t = traverseDownTm (desugarTm 0) t

desugarTm :: Index -> TermNode -> UpdatedTmArrTm
desugarTm c t = let tm = getTm t; fi = getFI t; desugarTm' = desugarTm c in
  UpdatedTmArrTm $
  case tm of
    TmVar l k x -> (TermNode fi $ TmVar (l + (c - k)) c x, desugarTm', desugarTm')
    TmAbs x ty t1 -> (t, desugarTm (c + 1), desugarTm')
    TmLet p t1 t2 -> (t, desugarTm (c + (length $ namesOfPattern p)), desugarTm')
    TmCase t1 ts -> (t, desugarTm (c + 1), desugarTm')
    TmSeq t1 t2 -> (TermNode fi $ TmApp (TermNode fi $ TmAbs "x" TyUnit t2) t1, desugarTm', desugarTm')
    TmWildCard ty t2 -> (TermNode fi $ TmAbs "x" ty t2, desugarTm', desugarTm')
    TmAscribe t1 ty -> (TermNode fi $ TmApp (TermNode fi $ TmAbs "x" ty (TermNode fi $ TmVar 0 (c + 1) "[Helper: DONTPRINTME]")) t1, desugarTm', desugarTm')
    _ -> (t, desugarTm', desugarTm')

data TypingMethod
  = Check
  | Infer
  | OnlyInfer
  | TypingError String
  deriving (Eq, Show)

getTypingMethod :: TermNode -> TypingMethod
getTypingMethod t = let tm = getTm t in
  case tm of
    TmVar _ _ _ -> Infer
    TmApp t1 t2 -> Infer || getTypingMethod t1 || getTypingMethod t2
    TmAbs _ TyUnknown t1 -> OnlyInfer || getTypingMethod t1
    TmAbs _ _ t1 -> Check || getTypingMethod t1
    TmTrue -> Check
    TmFalse -> Check
    TmIf t1 t2 t3 -> Check || getTypingMethod t1 || getTypingMethod t2 || getTypingMethod t3
    TmZero -> Check
    TmSucc t1 -> Check || getTypingMethod t1
    TmPred t1 -> Check || getTypingMethod t1
    TmIsZero t1 -> Check || getTypingMethod t1
    TmUnit -> Check
    TmSeq t1 t2 -> Check || getTypingMethod t1 || getTypingMethod t2
    TmWildCard _ t2 -> Check || getTypingMethod t2
    TmAscribe t1 _ -> Check || getTypingMethod t1
    TmLet _ t1 t2 -> Check || getTypingMethod t1 || getTypingMethod t2
    TmRecord ts -> Check || foldr (||) Check (map (getTypingMethod . snd) ts)
    TmProj t1 _ -> Check || getTypingMethod t1
    TmVariant _ t1 _ -> Check || getTypingMethod t1
    TmCase t1 ts -> Check || getTypingMethod t1 || foldr (||) Check (map (getTypingMethod . snd . snd) ts)
    TmFix t1 -> Check || getTypingMethod t1
    TmNil _ -> Check
    TmCons _ t1 t2 -> Check || getTypingMethod t1 || getTypingMethod t2
    TmIsNil _ t1 -> Check || getTypingMethod t1
    TmHead _ t1 -> Check || getTypingMethod t1
    TmTail _ t1 -> Check || getTypingMethod t1
  where (||) :: TypingMethod -> TypingMethod -> TypingMethod
        (||) x Infer = x
        (||) Infer y = y
        (||) x y | x == y = x
                 | otherwise = TypingError "Can't typecheck when the type of an abstraction is unknown; also can't infer from extended terms."

findTypeErrors' :: Type -> String
findTypeErrors' t = intercalate "\n" $ findTypeErrors t

findTypeErrors :: Type -> [String]
findTypeErrors t =
  case t of
    TyErr e -> [e]
    TyVar _ -> []
    _ | elem t [TyBool, TyNat, TyUnit, TyUnknown] -> []
    TyArr ty1 ty2 -> findTypeErrors ty1 ++ findTypeErrors ty2
    TyRecord tys -> concat $ map findTypeErrors $ map snd tys
    TyVariant tys -> concat $ map findTypeErrors $ map snd tys
    TyList ty -> findTypeErrors ty

findTermErrors' :: TermNode -> String
findTermErrors' t = intercalate "\n" $ findTermErrors t

findTermErrors :: TermNode -> [String]
findTermErrors t = let tm = getTm t in
  case tm of
    TmErr e -> [e]
    TmVar _ _ _ -> []
    TmApp t1 t2 -> findTermErrors t1 ++ findTermErrors t2
    TmAbs _ TyUnknown t1 -> findTermErrors t1
    TmAbs _ _ t1 -> findTermErrors t1
    TmTrue -> []
    TmFalse -> []
    TmIf t1 t2 t3 -> findTermErrors t1 ++ findTermErrors t2 ++ findTermErrors t3
    TmZero -> []
    TmSucc t1 -> findTermErrors t1
    TmPred t1 -> findTermErrors t1
    TmIsZero t1 -> findTermErrors t1
    TmUnit -> []
    TmSeq t1 t2 -> findTermErrors t1 ++ findTermErrors t2
    TmWildCard _ t2 -> findTermErrors t2
    TmAscribe t1 _ -> findTermErrors t1
    TmLet _ t1 t2 -> findTermErrors t1 ++ findTermErrors t2
    TmRecord ts -> foldr (++) [] (map (findTermErrors . snd) ts)
    TmProj t1 _ -> findTermErrors t1
    TmVariant _ t1 _ -> findTermErrors t1
    TmCase t1 ts -> findTermErrors t1 ++ foldr (++) [] (map (findTermErrors . snd . snd) ts)
    TmFix t1 -> findTermErrors t1
    TmNil _ -> []
    TmCons _ t1 t2 -> findTermErrors t1 ++ findTermErrors t2
    TmIsNil _ t1 -> findTermErrors t1
    TmHead _ t1 -> findTermErrors t1
    TmTail _ t1 -> findTermErrors t1

findDisplayErrors' :: String -> String
findDisplayErrors' s =
  case findDisplayErrors s [] of
    [] -> s
    s' -> intercalate "\n" s'

findDisplayErrors :: String -> String -> [String]
findDisplayErrors [] [] = []
findDisplayErrors [] ys = error "Someone needs to fix this, there's an error message which does not have its identifiers matching."
findDisplayErrors ('#':xs) [] = findDisplayErrors xs ['#']
findDisplayErrors ('#':xs) ys = ((\s -> case s of [] -> []; (x:xs) -> xs) $ reverse ys):findDisplayErrors xs []
findDisplayErrors (x:xs) [] = findDisplayErrors xs []
findDisplayErrors (x:xs) ys = findDisplayErrors xs (x:ys)

