module Typing where

import Syntax
import Display

import Data.List

typeOf' :: TermNode -> Type
typeOf' t = typeOf [] t

typeOf :: Context -> TermNode -> Type
typeOf ctx t =
  case tm of
    TmVar k l x -> getTypeFromContext ctx k
    TmAbs x ty t1 -> TyArr ty $ typeOf ((x, ty):ctx) t1
    TmApp t1 t2 ->
      let tyT1 = typeOf' t1; tyT2 = typeOf' t2 in
      case tyT1 of
        TyArr tyT11 tyT12 -> check3 tyT11 tyT2 tyT12 $ tmAppErr2 tyT11 tyT2
        _ -> TyErr $ tmAppErr1 tyT1 
    TmTrue -> TyBool
    TmFalse -> TyBool
    TmIf t1 t2 t3 ->
      let tyT1 = typeOf' t1; tyT2 = typeOf' t2; tyT3 = typeOf' t3 in
      if tyT1 == TyBool
        then check2 tyT2 tyT3 $ tmIfErr2 tyT2 tyT3
        else TyErr $ tmIfErr1 tyT1
    TmZero -> TyNat
    TmSucc t1 -> let tyT1 = typeOf' t1 in check3 tyT1 TyNat TyNat $ tmSuccErr tyT1
    TmPred t1 -> let tyT1 = typeOf' t1 in check3 tyT1 TyNat TyNat $ tmPredErr tyT1
    TmIsZero t1 -> let tyT1 = typeOf' t1 in check3 tyT1 TyNat TyBool $ tmIsZeroErr tyT1
    TmUnit -> TyUnit
    TmAscribe t1 ty -> let tyT1 = typeOf' t1 in check2 tyT1 ty $ tmAscribeErr tyT1 ty
    TmSeq t1 t2 -> let tyT1 = typeOf' t1 in check3 tyT1 TyUnit (typeOf' t2) $ tmSeqErr tyT1
    TmWildCard ty t1 -> TyArr ty $ typeOf' t1
    TmLet p t1 t2 ->
      let tyT1 = typeOf' t1
          d = typeOfPattern p tyT1
       in typeOf (d ++ ctx) t2
    TmProj t1 x -> let tyT1 = typeOf' t1 in
      case tyT1 of
        TyRecord tys -> fromMaybe $ lookup x tys
        _ -> TyErr tmProjErr
    TmRecord ts -> TyRecord $ map (\(x, y) -> (x, typeOf' y)) ts
    TmVariant x t1 ty@(TyVariant ty1) ->
      let tyMatch = fromMaybe $ lookup x ty1
          tyT1 = typeOf' t1
       in check3 tyMatch tyT1 ty $ tmVariantErr tyT1 tyMatch
    TmCase t1 ts -> let tyT1 = typeOf' t1 in
      case tyT1 of
        (TyVariant tys) ->
          (\(x:xs) -> if all (== x) xs then x else TyErr tmCaseErr2)
            $ map (\((x, (y, z)), ty) -> typeOf ((y, ty):ctx) z)
            $ zip ts (map snd tys)
        _ -> TyErr $ tmCaseErr1 tyT1
    TmFix t1 -> let tyT1 = typeOf' t1 in
      case tyT1 of
        TyArr ty1 ty2 | ty1 == ty2 -> ty1
        _ -> TyErr $ tmFixErr tyT1
    TmNil ty -> TyList ty
    TmCons ty t1 t2 -> let tyT1 = typeOf' t1; tyT2 = typeOf' t2 in
      if tyT1 == ty && tyT2 == TyList ty
        then TyList ty
        else TyErr $ tmConsErr tyT1 tyT2 ty
    TmIsNil ty t1 -> let tyT1 = typeOf' t1 in check3 tyT1 (TyList ty) TyBool $ tmIsNilErr tyT1 ty
    TmHead ty t1 -> let tyT1 = typeOf' t1 in check3 tyT1 (TyList ty) ty $ tmHeadErr tyT1 ty
    TmTail ty t1 -> let tyT1 = typeOf' t1 in check2 tyT1 (TyList ty) $ tmTailErr tyT1 ty
    _ -> TyErr ("No rule applies" ++ showFileInfo fi)
  where tm = getTm t
        fi = getFI t
        typeOf' = typeOf ctx
        check3 :: Type -> Type -> Type -> String -> Type
        check3 ty1 ty2 ty3 err = if ty1 == ty2 then ty3 else TyErr err
        check2 :: Type -> Type -> String -> Type
        check2 ty1 ty2 err = check3 ty1 ty2 ty2 err

        tmAppErr1 tyT1 = "TmApp: expected TyArr, but got " ++ showType tyT1 ++ showFileInfo fi
        tmAppErr2 tyT11 tyT2 = "TmApp: type mismatch, where tyT11 is " ++ showType tyT11 ++ " and tyT2 is " ++ showType tyT2 ++ showFileInfo fi
        tmIfErr1 tyT1 = "TmIf: expected Bool, but got " ++ showType tyT1 ++ showFileInfo fi
        tmIfErr2 tyT2 tyT3 = "TmIf: type mismatch, where tyT2 is " ++ showType tyT2 ++ " and tyT3 is " ++ showType tyT3 ++ showFileInfo fi
        tmSuccErr tyT1 = "TmSucc: expected Nat, but got " ++ showType tyT1 ++ showFileInfo fi
        tmPredErr tyT1 = "TmPred: expected Nat, but got " ++ showType tyT1 ++ showFileInfo fi
        tmIsZeroErr tyT1 = "TmIsZero: expected Nat, but got " ++ showType tyT1 ++ showFileInfo fi
        tmAscribeErr tyT1 ty = "TmAscribe: expected " ++ showType ty ++ " but got " ++ showType tyT1 ++ showFileInfo fi
        tmSeqErr tyT1 = "TmSeq: expected Unit, but got " ++ showType tyT1 ++ showFileInfo fi
        tmProjErr = "TmProj: tyT1 is not of the record kind"
        tmVariantErr tyT1 tyMatch = "TmVariant: type mismatch, where tyT1 is " ++ showType tyT1 ++ " and tyMatch is " ++ showType tyMatch ++ showFileInfo fi
        tmCaseErr1 tyT1 = "TmCase: expected tyT1 to be of the TyVariant kind, but got: " ++ showType tyT1
        tmCaseErr2 = "TmCase: not all match constructors match type!"
        tmFixErr tyT1 = "TmFix: tyT1 is not of type form T->T for some T, instead it's " ++ showType tyT1
        tmConsErr tyT1 tyT2 ty = "TmCons: tyT1 not equal to ty or tyT2 not equal to TyList ty, tyT1 is " ++ showType tyT1 ++ ", tyT2 is " ++ showType tyT2 ++ "and ty is " ++ showType ty
        tmIsNilErr tyT1 ty = "TmIsNil: tyT1 not equal to List ty, tyT1 is " ++ showType tyT1 ++ " and ty is " ++ showType ty
        tmHeadErr tyT1 ty = "TmHead: tyT1 not equal to List ty, tyT1 is " ++ showType tyT1 ++ " and ty is " ++ showType ty
        tmTailErr tyT1 ty = "TmTail: tyT1 not equal to List ty, tyT1 is " ++ showType tyT1 ++ " and ty is " ++ showType ty

getTypeFromContext :: Context -> Index -> Type
getTypeFromContext ctx ind | ind < length ctx = snd $ (ctx !! ind)
                           | otherwise = TyErr "TmVar: no type context for variable"

type Subst = (Type, Type)
type EquationSet = [(Type, Type)]
type TContext = [(Index, Type)]
type TypeVarID = Int

lowerIndexTy :: Type -> Index -> Type
lowerIndexTy t k =
  case t of
    TyVar n -> TyVar $ n - k + 1
    TyArr t1 t2 -> TyArr (lowerIndexTy t1 k) (lowerIndexTy t2 k)

findLeastIndexTy :: Type -> Index
findLeastIndexTy t =
  case t of
    TyVar n -> n
    TyArr t1 t2 -> min (findLeastIndexTy t1) (findLeastIndexTy t2) 

getFVTm' :: TermNode -> [Index]
getFVTm' t = getFVTm 0 t

getFVTm :: Index -> TermNode -> [Index]
getFVTm n t = let tm = getTm t in
  case tm of
    TmVar l _ _ -> if l >= n then [l - n] else []
    TmApp t1 t2 -> getFVTm n t1 ++ getFVTm n t2
    TmAbs _ _ t1 -> getFVTm (n + 1) t1

getFVTy :: Type -> [Type]
getFVTy t =
  case t of
    TyVar n -> [t]
    TyArr ty1 ty2 -> getFVTy ty1 ++ getFVTy ty2

substCompose :: [Subst] -> [Subst]
substCompose [] = []
substCompose (x:xs) =
  let substFun = map (\(a, b) -> if fst x == b then (a, snd x) else (a, b))
   in x:(substCompose $ substFun xs)

tyVarOccurs :: Type -> Type -> Bool
tyVarOccurs t1 t2 =
  case t2 of
    TyVar _ -> t1 == t2
    TyArr ty1 ty2 -> tyVarOccurs t1 ty1 || tyVarOccurs t1 ty2

substType :: [Subst] -> Type -> Type
substType s t =
  case t of
    TyVar n -> let s' = lookup t s in
      if s' == Nothing
        then t
        else fromMaybe s'
    TyArr ty1 ty2 -> TyArr (substType s ty1) (substType s ty2)

substTContext :: [Subst] -> TContext -> TContext
substTContext s ctx = map (\(x, y) -> (x, substType s y)) ctx

unify1 :: Type -> Type -> Either String [Subst]
unify1 ty1 ty2 =
  case (ty1, ty2) of
    (TyVar _, _) ->
      if ty1 == ty2
        then Right []
      else if not $ tyVarOccurs ty1 ty2
        then Right [(ty1, ty2)]
        else Left $ unify1EqErr ty1 ty2
    (TyArr _ _, TyVar _) -> unify1 ty2 ty1
    (TyArr ty11 ty12, TyArr ty21 ty22) ->
       case unify1 ty11 ty21 of
         Left e -> Left e
         Right s' ->
           case unify1 (substType s' ty12) (substType s' ty22) of
             Left e -> Left e
             Right s'' -> Right $ substCompose (s' ++ s'')
  where unify1EqErr ty1 ty2 = "Unification: failed to unify because " ++ showType ty1 ++ " is not equal to " ++ showType ty2


unify' :: EquationSet -> Either String [Subst]
unify' eq =
  case unify eq of
    Left e -> Left e
    Right s -> Right $ substCompose s

unify :: EquationSet -> Either String [Subst]
unify [] = Right []
unify ((t1, t2):xs) =
  case unify xs of
    Left e -> Left e
    Right s -> let xs' = substCompose s in
      case unify1 (substType xs' t1) (substType xs' t2) of
        Left e -> Left e
        Right x' -> Right $ x' ++ xs'

inferT' :: TermNode -> Either String (TContext, Type)
inferT' t =
  case inferT t 0 of
      Left e -> Left e
      Right (ctx, ty, _) ->
        let k = findLeastIndexTy ty
            ty' = lowerIndexTy ty k
            ctx' = map (\(x, y) -> (x, lowerIndexTy y k)) ctx
         in Right (ctx', ty')

inferT :: TermNode -> TypeVarID -> Either String (TContext, Type, TypeVarID)
inferT t n = let tm = getTm t in
  case tm of
    TmVar l _ _ -> Right ([(l, TyVar n)], TyVar n, n + 1)
    TmApp t1 t2 ->
      case inferT t1 n of
        Left e -> Left e
        Right (ctx1, ty1, n') ->
          case inferT t2 n' of
            Left e -> Left e
            Right (ctx2, ty2, n'') ->
              let freeVars = getFVTm' t1 `intersect` getFVTm' t2
                  getEqsFromCtx = (map snd) . filter (\(x, y) -> elem x freeVars)
                  equations = zip (getEqsFromCtx ctx1) (getEqsFromCtx ctx2)
                  s = unify' ((ty1, TyArr ty2 (TyVar n'')):equations)
               in
                case s of
                  Left e -> Left e
                  Right s' -> Right (substTContext s' (ctx1 ++ ctx2), substType s' $ TyVar n'', n'' + 1)
    TmAbs _ _ t1 ->
      case inferT t1 n of
        Left e -> Left e
        Right (ctxN, ty, n') ->
          let ctxN' = map (\(x, y) -> (x - 1, y)) ctxN
              pair = lookup (-1) ctxN'
           in Right $
            if pair == Nothing
              then (ctxN', TyArr (TyVar n') ty, n' + 1)
              else (filter ((/= (-1)) . fst) ctxN', TyArr (fromMaybe pair) ty, n')
