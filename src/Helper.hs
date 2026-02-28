module Helper where

import Syntax

import Data.List

newtype UpdatedTmArrTm = UpdatedTmArrTm
  { updateTmArrTm :: (TermNode, TermNode -> UpdatedTmArrTm, TermNode -> UpdatedTmArrTm, Type -> Type) }

traverseDownTm :: (TermNode -> UpdatedTmArrTm) -> TermNode -> TermNode
traverseDownTm f t = TermNode fi $
  case tm of
    TmVar k l x -> tm
    TmVarRaw x -> tm
    TmAbs x ty t1 -> TmAbs x (fTy ty) (traverseTm' t1)
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
    TmAscribe t1 ty -> TmAscribe (traverseTm' t1) (fTy ty)
    TmLet p t1 t2 -> TmLet p (traverseTm'' t1) (traverseTm' t2)
    TmRecord ts -> TmRecord $ map (\(x, y) -> (x, traverseTm' y)) ts
    TmProj t1 x -> TmProj (traverseTm' t1) x
    TmVariant x t1 ty -> TmVariant x (traverseTm' t1) (fTy ty)
    TmCase t1 ts -> TmCase (traverseTm'' t1) $ map (\(x, (y, z)) -> (x, (y, traverseTm' z))) ts
    TmFix t1 -> TmFix $ traverseTm' t1
    TmNil ty -> tm
    TmCons ty t1 t2 -> TmCons (fTy ty) (traverseTm' t1) (traverseTm' t2)
    TmIsNil ty t1 -> TmIsNil (fTy ty) $ traverseTm' t1
    TmHead ty t1 -> TmHead (fTy ty) $ traverseTm' t1
    TmTail ty t1 -> TmTail (fTy ty) $ traverseTm' t1
    TmTyAbs x t1 -> TmTyAbs x $ traverseTm' t1
    TmTyApp t1 ty -> TmTyApp (traverseTm' t1) (fTy ty)
    TmPack ty1 t1 ty2 -> TmPack (fTy ty1) (traverseTm' t1) (fTy ty2)
    TmUnpack x1 x2 t1 t2 -> TmUnpack x1 x2 (traverseTm'' t1) (traverseTm' t2)
  where tm = getTm t'
        fi = getFI t'
        traverseTm'  = traverseDownTm f'
        traverseTm'' = traverseDownTm f''
        (t', f', f'', fTy) = updateTmArrTm $ f t

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
    TmTyAbs _ _ -> True
    TmPack _ t1 _ | isVal t1 -> True
    _ -> False

getNumFromVal :: TermNode -> (Int, Bool)
getNumFromVal t = let tm = getTm t in
  case tm of
    TmZero -> (0, True)
    TmSucc t1 -> let (n, b) = getNumFromVal t1 in (n + 1, b)
    _ -> (0, False)

isPattern :: TermNode -> Bool
isPattern t = let tm = getTm t in
  case tm of
    TmVar _ _ _ -> True
    TmRecord ts -> and $ map (isPattern . snd) ts
    _ -> False

typingEvalSubst :: Type -> Type -> Type
typingEvalSubst s t = tyShift' (-1) (tySubst' 0 (tyShift' 1 s) t)

tyShift' :: Index -> Type -> Type
tyShift' d t = tyShift 0 d t

tyShift :: Index -> Index -> Type -> Type
tyShift c d t =
  case t of
    TyBool -> TyBool
    TyNat -> TyNat
    TyUnit -> TyUnit
    TyArr t1 t2 -> TyArr (tyShift' t1) (tyShift' t2)
    TyRecord ts -> TyRecord $ map (\(x, y) -> (x, tyShift' y)) ts
    TyVariant ts -> TyVariant $ map (\(x, y) -> (x, tyShift' y)) ts
    TyList t1 -> TyList $ tyShift' t1
    TyVarF k l x -> TyVarF (if k < c then k else k + d) (l + d) x
    TyForAll x t1 -> TyForAll x $ tyShift (c + 1) d t1
    TyForSome x t1 -> TyForSome x $ tyShift (c + 1) d t1
    TyErr e -> TyErr e
    _ -> TyErr $ "Helper, tyShift: type is not applicable: " ++ show t
  where tyShift' = tyShift c d

tySubst' :: Index -> Type -> Type -> Type
tySubst' j s t = tySubst 0 j s t

tySubst :: Index -> Index -> Type -> Type -> Type
tySubst c j s t =
  case t of
    TyBool -> TyBool
    TyNat -> TyNat
    TyUnit -> TyUnit
    TyArr t1 t2 -> TyArr (tySubst' t1) (tySubst' t2)
    TyRecord ts -> TyRecord $ map (\(x, y) -> (x, tySubst' y)) ts
    TyVariant ts -> TyVariant $ map (\(x, y) -> (x, tySubst' y)) ts
    TyList t1 -> TyList $ tySubst' t1
    TyVarF k l x -> if k == j + c then tyShift' (j + c) s else t
    TyForAll x t1 -> TyForAll x $ tySubst (c + 1) j s t1
    TyForSome x t1 -> TyForSome x $ tySubst (c + 1) j s t1
    TyErr e -> TyErr e
    _ -> TyErr $ "Helper, tySubst: type is not applicable; " ++ show t
  where tySubst' = tySubst c j s

shift' :: Index -> Index -> TermNode -> TermNode
shift' c d t = traverseDownTm (shift c d) t

shift :: Index -> Index -> TermNode -> UpdatedTmArrTm
shift c d t = let tm = getTm t; fi = getFI t; shift' = shift c d in
  UpdatedTmArrTm $
  case tm of
    TmVar k l x -> (TermNode fi $ TmVar (if k < c then k else k + d) (l + d) x, id', id', tyShift' d)
    TmAbs x ty t1 -> (t, shift (c + 1) d, shift', tyShift c d)
    TmWildCard ty t2 -> (t, shift (c + 1) d, shift', tyShift c d)
    TmLet p t1 t2 -> (t, shift (c + (length $ namesOfPattern p)) d, shift', tyShift c d)
    TmCase t1 ts -> (t, shift (c + 1) d, shift', tyShift c d)
    TmTyAbs x t1 -> (t, shift (c + 1) d, shift', tyShift c d)
    TmUnpack x1 x2 t1 t2 -> (t, shift (c + 2) d, shift', tyShift c d)
    _ -> (t, shift', shift', tyShift c d)

evalTyTermSubst :: Type -> TermNode -> TermNode
evalTyTermSubst s t = shift' 0 (-1) (tyTermSubst' 0 (tyShift' 1 s) t)

tyTermSubst' :: Index -> Type -> TermNode -> TermNode
tyTermSubst' j s t = traverseDownTm (tyTermSubst 0 j s) t

tyTermSubst :: Index -> Index -> Type -> TermNode -> UpdatedTmArrTm
tyTermSubst c j s t = let tm = getTm t; fi = getFI t; tyTermSubst' = tyTermSubst c j s in
  UpdatedTmArrTm $
  case tm of
    TmAbs x ty t1 -> (t, tyTermSubst (c + 1) j s, tyTermSubst', tySubst' (c + j) s)
    TmWildCard ty t2 -> (t, tyTermSubst (c + 1) j s, tyTermSubst', tySubst' (c + j) s)
    TmLet p t1 t2 -> (t, tyTermSubst (c + (length $ namesOfPattern p)) j s, tyTermSubst', tySubst' (c + j) s)
    TmCase t1 ts -> (t, tyTermSubst (c + 1) j s, tyTermSubst', tySubst' (c + j) s)
    TmTyAbs x t1 -> (t, tyTermSubst (c + 1) j s, tyTermSubst', tySubst' (c + j) s)
    TmUnpack x1 x2 t1 t2 -> (t, tyTermSubst (c + 2) j s, tyTermSubst', tySubst' (c + j) s)
    _ -> (t, tyTermSubst', tyTermSubst', tySubst' (c + j) s)

subst' :: Index -> TermNode -> TermNode -> TermNode
subst' j s t = traverseDownTm (subst 0 j s) t

subst :: Index -> Index -> TermNode -> TermNode -> UpdatedTmArrTm
subst c j s t = let tm = getTm t; subst' = subst c j s in
  UpdatedTmArrTm $
  case tm of
    TmVar k l x -> (if k == j + c then shift' 0 (j + c) s else t, id', id', id :: Type -> Type)
    TmAbs x ty t1 -> (t, subst (c + 1) j s, subst', id :: Type -> Type)
    TmWildCard ty t2 -> (t, subst (c + 1) j s, subst', id :: Type -> Type)
    TmLet p t1 t2 -> (t, subst (c + (length $ namesOfPattern p)) j s, subst', id :: Type -> Type)
    TmCase t1 ts -> (t, subst (c + 1) j s, subst', id :: Type -> Type)
    TmTyAbs x t1 -> (t, subst (c + 1) j s, subst', id :: Type -> Type)
    TmUnpack x1 x2 t1 t2 -> (t, subst (c + 2) j s, subst', id :: Type -> Type)
    _ -> (t, subst', subst', id :: Type -> Type)

genIndex' :: TermNode -> TermNode
genIndex' t = traverseDownTm (genIndex []) t

genIndex :: [Name] -> TermNode -> UpdatedTmArrTm
genIndex ctx t = let tm = getTm t; fi = getFI t; genIndex' = genIndex ctx in
  UpdatedTmArrTm $
  case tm of
    TmVarRaw x -> (TermNode fi $ TmVar (length $ takeWhile (/= x) ctx) (length ctx) x, genIndex', genIndex', id :: Type -> Type)
    TmAbs x ty t1 -> (TermNode fi $ TmAbs x (genIndexType ctx ty) t1, genIndex (x:ctx), genIndex', id :: Type -> Type)
    TmWildCard ty t2 -> (TermNode fi $ TmWildCard (genIndexType ctx ty) t2, genIndex ("":ctx), genIndex', id :: Type -> Type)
    TmRecord ts -> (TermNode fi $ TmRecord (map (\((x, y), k) -> (case x of "" -> show k; _ -> x, y)) $ zip ts [1..]), genIndex', genIndex', id :: Type -> Type)
    TmLet p@(PRecord ps) t1 t2 ->
      ( TermNode fi
        $ TmLet (PRecord (map (\((x, y), k) -> (case x of "" -> show k; _ -> x, y)) $ zip ps [1..])) t1 t2
      , genIndex (namesOfPattern p ++ ctx)
      , genIndex'
      , id :: Type -> Type
      )
    TmLet p t1 t2 -> (t, genIndex (namesOfPattern p ++ ctx), genIndex', id :: Type -> Type)
    TmCase t1 ts -> (TermNode fi $ TmCase (traverseDownTm (genIndex ctx) t1) (map (\(x, (y, z)) -> (x, (y, traverseDownTm (genIndex (y:ctx)) z )) ) ts), id', id', id :: Type -> Type)
    TmVariant x t1 ty -> (TermNode fi $ TmVariant x t1 (genIndexType ctx ty), genIndex', genIndex', id :: Type -> Type)
    TmTyAbs x t1 -> (t, genIndex (x:ctx), genIndex', id :: Type -> Type)
    TmTyApp t1 ty -> (TermNode fi $ TmTyApp t1 (genIndexType ctx ty), genIndex', genIndex', id :: Type -> Type)
    TmUnpack x1 x2 t1 t2 -> (t, genIndex (x2:x1:ctx), genIndex', id :: Type -> Type)
    TmPack ty1 t1 ty2 -> (TermNode fi $ TmPack (genIndexType ctx ty1) t1 (genIndexType ctx ty2), genIndex', genIndex', id :: Type -> Type)
    _ -> (t, genIndex', genIndex', id :: Type -> Type)
  where genIndexType :: [Name] -> Type -> Type
        genIndexType ctx ty =
          case ty of
            TyRecord tys -> TyRecord $ map (\((x, y), k) -> (case x of "" -> show k; _ -> x, genIndexType ctx y)) $ zip tys [1..]
            TyVariant tys -> TyVariant $ map (\(x, y) -> (x, genIndexType ctx y)) tys
            TyArr t1 t2 -> TyArr (genIndexType ctx t1) (genIndexType ctx t2)
            TyForAll x t1 -> TyForAll x (genIndexType (x:ctx) t1)
            TyForSome x t1 -> TyForSome x (genIndexType (x:ctx) t1)
            TyVarFRaw x -> TyVarF (length $ takeWhile (/= x) ctx) (length ctx) x
            _ -> ty

id' :: TermNode -> UpdatedTmArrTm
id' t = UpdatedTmArrTm (t, id', id', id :: Type -> Type)

desugarTm' :: TermNode -> TermNode
desugarTm' t = traverseDownTm (desugarTm 0) t

desugarTm :: Index -> TermNode -> UpdatedTmArrTm
desugarTm c t = let tm = getTm t; fi = getFI t; desugarTm' = desugarTm c in
  UpdatedTmArrTm $
  case tm of
    TmVar l k x -> (TermNode fi $ TmVar (l + (c - k)) c x, desugarTm', desugarTm', id :: Type -> Type)
    TmAbs x ty t1 -> (t, desugarTm (c + 1), desugarTm', id :: Type -> Type)
    TmWildCard ty t2 -> (TermNode fi $ TmAbs "x" ty t2, desugarTm (c + 1), desugarTm', id :: Type -> Type)
    TmCase t1 ts -> (t, desugarTm (c + 1), desugarTm', id :: Type -> Type)
    TmLet p t1 t2 -> (t, desugarTm (c + (length $ namesOfPattern p)), desugarTm', id :: Type -> Type)
    TmSeq t1 t2 -> (TermNode fi $ TmApp (TermNode fi $ TmAbs "x" TyUnit t2) t1, desugarTm', desugarTm', id :: Type -> Type)
    TmAscribe t1 ty -> (TermNode fi $ TmApp (TermNode fi $ TmAbs "x" ty (TermNode fi $ TmVar 0 (c + 1) "[Helper: DONTPRINTME]")) t1, desugarTm', desugarTm', id :: Type -> Type)
    TmTyAbs x t1 -> (t, desugarTm (c + 1), desugarTm', id :: Type -> Type)
    TmUnpack _ _ t1 t2 -> (t, desugarTm (c + 2), desugarTm', id :: Type -> Type)
    TmPack _ t1 _ -> (t, desugarTm', desugarTm', id :: Type -> Type)
    _ -> (t, desugarTm', desugarTm', id :: Type -> Type)

data TypingMethod
  = Check
  | Infer InferMethod
  | OnlyInfer InferMethod
  | TypingError String
  deriving (Eq, Show)

data InferMethod
  = AlgorithmT
  | AlgorithmW
  deriving (Eq, Show)

getTypingMethod :: TermNode -> TypingMethod
getTypingMethod t = let tm = getTm t in
  case tm of
    TmVar _ _ _ -> Infer AlgorithmT
    TmApp t1 t2 -> Infer AlgorithmT || getTypingMethod t1 || getTypingMethod t2
    TmAbs _ TyUnknown t1 -> OnlyInfer AlgorithmT || getTypingMethod t1
    TmAbs _ _ t1 -> Check || getTypingMethod t1
    TmLet (PVar _) t1 t2 -> Infer AlgorithmW || getTypingMethod t1 || getTypingMethod t2
    TmLet _ t1 t2 -> Check || getTypingMethod t1 || getTypingMethod t2
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
    TmTyAbs x t1 -> Check || getTypingMethod t1
    TmTyApp t1 ty -> Check || getTypingMethod t1
    TmUnpack _ _ t1 t2 -> Check || getTypingMethod t1 || getTypingMethod t2
    TmPack _ t1 _ -> Check || getTypingMethod t1
  where (||) :: TypingMethod -> TypingMethod -> TypingMethod
        (||) (OnlyInfer _) (Infer AlgorithmW) = OnlyInfer AlgorithmW
        (||) (Infer AlgorithmW) (OnlyInfer _) = OnlyInfer AlgorithmW
        (||) (Infer AlgorithmW) (Infer _) = Infer AlgorithmW
        (||) (Infer _) (Infer AlgorithmW) = Infer AlgorithmW
        (||) (Infer _) (Infer _) = Infer AlgorithmT
        (||) (OnlyInfer AlgorithmW) (OnlyInfer _) = OnlyInfer AlgorithmW
        (||) (OnlyInfer _) (OnlyInfer AlgorithmW) = OnlyInfer AlgorithmW
        (||) (OnlyInfer AlgorithmW) (Infer _) = OnlyInfer AlgorithmW
        (||) (Infer _) (OnlyInfer AlgorithmW) = OnlyInfer AlgorithmW
        (||) (OnlyInfer _) (OnlyInfer _) = OnlyInfer AlgorithmT
        (||) x (Infer _) = x
        (||) (Infer _) y = y
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
    TyVarF _ _ _ -> []
    TyForAll _ ty -> findTypeErrors ty
    TyForSome _ ty -> findTypeErrors ty

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
    TmTyAbs x t1 -> findTermErrors t1
    TmTyApp t1 ty -> findTermErrors t1
    TmUnpack _ _ t1 t2 -> findTermErrors t1 ++ findTermErrors t2
    TmPack _ t1 _ -> findTermErrors t1

findDisplayErrors' :: String -> Either String String
findDisplayErrors' s =
  case findDisplayErrors s [] of
    [] -> Right s
    s' -> Left $ intercalate "\n" s'

findDisplayErrors :: String -> String -> [String]
findDisplayErrors [] [] = []
findDisplayErrors [] ys = error "Someone needs to fix this, there's an error message which does not have its identifiers matching."
findDisplayErrors ('#':xs) [] = findDisplayErrors xs ['#']
findDisplayErrors ('#':xs) ys = ((\s -> case s of [] -> []; (x:xs) -> xs) $ reverse ys):findDisplayErrors xs []
findDisplayErrors (x:xs) [] = findDisplayErrors xs []
findDisplayErrors (x:xs) ys = findDisplayErrors xs (x:ys)

