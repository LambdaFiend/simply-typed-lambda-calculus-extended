module Typing where

import Syntax
import Display
import Helper

import Data.List

typeOf' :: TermNode -> Type
typeOf' t = typeOf [] t

typeOf :: Context -> TermNode -> Type
typeOf ctx t =
  case tm of
    TmVar k l x -> tyShift' (k + 1) $ getTypeFromContext ctx k
    TmAbs x ty t1 -> TyArr ty $ tyShift' (-1) $ typeOf ((x, ty):ctx) t1
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
    TmWildCard ty t1 -> TyArr ty $ typeOf (("", ty):ctx) t1
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
    TmTyAbs x t1 -> TyForAll x $ typeOf ((x, TyUnknown):ctx) t1
    TmTyApp t1 ty -> let tyT1 = typeOf' t1 in
      case tyT1 of
        TyForAll _ tyT12 -> typingEvalSubst ty tyT12
        _ -> TyErr $ "(TmTyApp: expected TyForAll, but got " ++ showType' tyT1 ++ ")"
    TmPack ty1 t1 ty2 ->
      case ty2 of
        TyForSome _ ty21 ->
          let tyT1  = typeOf ctx t1
              tyT1' = typingEvalSubst ty1 ty21
           in check3 tyT1 tyT1' ty2 ("(TmPack: tyT1 not equal to tyT1, with tyT1\' as " ++ showType' tyT1 ++ " and tyT1\' as " ++ showType' tyT1' ++ ")")
        _ -> TyErr $ "(TmPack: expected TyForSome, but got " ++ showType' ty2 ++ ")"
    TmUnpack x1 x2 t1 t2 -> let tyT1 = typeOf ctx t1 in
      case tyT1 of
        TyForSome _ tyT11 -> tyShift' (-2) $ typeOf ((x2, tyT11):(x1, TyUnknown):ctx) t2
        _ -> TyErr $ "(TmUnpack: expected TyForSome, but got" ++ showType' tyT1 ++ ")"
    _ -> TyErr ("No rule applies" ++ showFileInfo fi)
  where tm = getTm t
        fi = getFI t
        typeOf' = typeOf ctx
        check3 :: Type -> Type -> Type -> String -> Type
        check3 ty1 ty2 ty3 err = if ty1 == ty2 then ty3 else TyErr err
        check2 :: Type -> Type -> String -> Type
        check2 ty1 ty2 err = check3 ty1 ty2 ty2 err
        tmAppErr1 tyT1 = "\n(TmApp: expected TyArr, but got " ++ showType [] tyT1 ++ showFileInfo fi ++ ")"
        tmAppErr2 tyT11 tyT2 = "\n(TmApp: type mismatch, where tyT11 is " ++ showType [] tyT11 ++ " and tyT2 is " ++ showType [] tyT2 ++ showFileInfo fi ++ ")"
        tmIfErr1 tyT1 = "\n(TmIf: expected Bool, but got " ++ showType [] tyT1 ++ showFileInfo fi ++ ")"
        tmIfErr2 tyT2 tyT3 = "\n(TmIf: type mismatch, where tyT2 is " ++ showType [] tyT2 ++ " and tyT3 is " ++ showType [] tyT3 ++ showFileInfo fi ++ ")"
        tmSuccErr tyT1 = "\n(TmSucc: expected Nat, but got " ++ showType [] tyT1 ++ showFileInfo fi ++ ")"
        tmPredErr tyT1 = "\n(TmPred: expected Nat, but got " ++ showType [] tyT1 ++ showFileInfo fi ++ ")"
        tmIsZeroErr tyT1 = "\n(TmIsZero: expected Nat, but got " ++ showType [] tyT1 ++ showFileInfo fi ++ ")"
        tmAscribeErr tyT1 ty = "\n(TmAscribe: expected " ++ showType [] ty ++ " but got " ++ showType [] tyT1 ++ showFileInfo fi ++ ")"
        tmSeqErr tyT1 = "\n(TmSeq: expected Unit, but got " ++ showType [] tyT1 ++ showFileInfo fi ++ ")"
        tmProjErr = "\n(TmProj: tyT1 is not of the record kind" ++ ")"
        tmVariantErr tyT1 tyMatch = "\n(TmVariant: type mismatch, where tyT1 is " ++ showType [] tyT1 ++ " and tyMatch is " ++ showType [] tyMatch ++ showFileInfo fi ++ ")"
        tmCaseErr1 tyT1 = "\n(TmCase: expected tyT1 to be of the TyVariant kind, but got: " ++ showType [] tyT1 ++ ")"
        tmCaseErr2 = "\n(TmCase: not all match constructors match type!" ++ ")"
        tmFixErr tyT1 = "\n(TmFix: tyT1 is not of type form T->T for some T, instead it's " ++ showType [] tyT1 ++ ")"
        tmConsErr tyT1 tyT2 ty = "\n(TmCons: tyT1 not equal to ty or tyT2 not equal to TyList ty, tyT1 is " ++ showType [] tyT1 ++ ", tyT2 is " ++ showType [] tyT2 ++ "and ty is " ++ showType [] ty ++ ")"
        tmIsNilErr tyT1 ty = "\n(TmIsNil: tyT1 not equal to List ty, tyT1 is " ++ showType [] tyT1 ++ " and ty is " ++ showType [] ty ++ ")"
        tmHeadErr tyT1 ty = "\n(TmHead: tyT1 not equal to List ty, tyT1 is " ++ showType [] tyT1 ++ " and ty is " ++ showType [] ty ++ ")"
        tmTailErr tyT1 ty = "\n(TmTail: tyT1 not equal to List ty, tyT1 is " ++ showType [] tyT1 ++ " and ty is " ++ showType [] ty ++ ")"

getTypeFromContext :: Context -> Index -> Type
getTypeFromContext ctx ind | ind >= 0 && ind < length ctx = snd $ (ctx !! ind)
                           | otherwise = TyErr "\n(TmVar: no type context for variable)"

type Subst = (Type, Type)
type EquationSet = [(Type, Type)]
type TyVarContext = [(Index, Type)]
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
    TyScheme quants ty -> getFVTy ty \\ quants

substCompose1 :: Subst -> Type -> Type
substCompose1 s@(TyVar n, b) ty =
  case ty of
    TyVar m -> if m == n then b else ty
    TyArr ty1 ty2 -> TyArr (substCompose1 s ty1) (substCompose1 s ty2)

substCompose :: [Subst] -> [Subst]
substCompose [] = []
substCompose (x:xs) =
  let substFun = map (\(a, b) -> (a, substCompose1 x b))
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
    TyScheme quants ty -> let s' = foldr deleteByFstSubst s quants in TyScheme quants $ substType s' ty

deleteByFstSubst :: Type -> [Subst] -> [Subst]
deleteByFstSubst x xs =
  case break (\(x', _) -> x == x') xs of
    (prev, _:next) -> prev ++ next
    _              -> xs

substTyVarContext :: [Subst] -> TyVarContext -> TyVarContext
substTyVarContext s ctx = map (\(x, y) -> (x, substType s y)) ctx

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
             Right s'' -> Right $ substCompose (s'' ++ s')
  where unify1EqErr ty1 ty2 = "Unification: failed to unify because " ++ showType [] ty1 ++ " is not equal to " ++ showType [] ty2


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

inferT' :: TermNode -> Either String (TyVarContext, Type)
inferT' t =
  case inferT t 0 of
      Left e -> Left e
      Right (ctx, ty, _) ->
        let k = findLeastIndexTy ty
            ty' = lowerIndexTy ty k
            ctx' = map (\(x, y) -> (x, lowerIndexTy y k)) ctx
         in Right (ctx', ty')

inferT :: TermNode -> TypeVarID -> Either String (TyVarContext, Type, TypeVarID)
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
                  Right s' -> Right (substTyVarContext s' (ctx1 ++ ctx2), substType s' $ TyVar n'', n'' + 1)
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
    _ -> Left "Wrong term for inference using algorithm T"

inferW' :: TermNode -> Either String ([Subst], Type)
inferW' t =
  case inferW [] t 0 of
    Left e -> Left e
    Right (s, ty, _) -> Right (s, ty)

inferW :: TyVarContext -> TermNode -> TypeVarID -> Either String ([Subst], Type, TypeVarID)
inferW ctx t n = let tm = getTm t in
  case tm of
    TmVar l _ _ ->
      if l >= 0 && l < length ctx
        then
          case snd (ctx !! l) of
            TyScheme quants ty1 ->
              let subst = map (\(x, y) -> (x, TyVar y)) $ zip quants [n..length quants + n - 1]
               in Right ([], substType subst ty1, n + length quants) 
            _ -> Left "Infer W: wrong type under context"
        else Left "Infer W: No context for variable"
    TmApp t1 t2 ->
      case inferW ctx t1 n of
        Left e -> Left e
        Right (s1, ty1, n') ->
          case inferW (substTyVarContext s1 ctx) t2 n' of
            Left e -> Left e
            Right (s2, ty2, n'') ->
              case unify1 (substType s2 ty1) $ TyArr ty2 (TyVar n'') of
                Left e -> Left e
                Right s3 -> Right (substCompose (s3 ++ s2 ++ s1), substType s3 $ TyVar n'', n'' + 1)
    TmAbs x _ t1 ->
      case inferW ((-1, TyScheme [] (TyVar n)):ctx) t1 (n + 1) of
        Left e -> Left e
        Right (s1, ty1, n') -> Right (s1, TyArr (substType s1 (TyVar n)) ty1, n')
    TmLet (PVar x) t1 t2 ->
      case inferW ctx t1 n of
        Left e -> Left e
        Right (s1, ty1, n') ->
            case inferW ((-1, TyScheme (closure (substTyVarContext s1 ctx) ty1) ty1):ctx) t2 n' of
              Left e -> Left e
              Right (s2, ty2, n'') -> Right (substCompose (s2 ++ s1), ty2, n'')
    _ -> Left "Wrong term for inference using algorithm W"

closure :: TyVarContext -> Type -> [Type]
closure ctx ty = getFVTy ty \\ concat (map (getFVTy . snd) ctx)
