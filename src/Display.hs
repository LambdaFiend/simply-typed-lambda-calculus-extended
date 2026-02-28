module Display where

import Lexer
import Syntax
import Helper

import Data.List

showTm' :: TermNode -> String
showTm' t = let (n , b) = getNumFromVal t in
  if isVal t && b
    then show n
    else removeOuterParens $ showTm [] t

showTm :: [Name] -> TermNode -> String
showTm ctx t = let tm = getTm t in
  case tm of
    TmVar k l x -> let ctxLength = length ctx in
      if l == ctxLength
        then getNameFromContext ctx k x
        else tmVarErr l ctxLength
    TmAbs x ty t1 ->
      let x' = fixName' x
       in "(" ++ "λ" ++ x' ++ ":" ++ showType ctx ty ++ "." ++ showTm (x':ctx) t1 ++ ")"
    TmApp t1 t2 -> "(" ++ showTm' t1 ++ " " ++ showTm' t2 ++ ")"
    TmSucc t1 -> "(" ++ "succ " ++ showTm' t1 ++ ")"
    TmPred t1 -> "(" ++ "pred " ++ showTm' t1 ++ ")"
    TmIsZero t1 -> "(" ++ "iszero " ++ showTm' t1 ++ ")"
    TmZero -> "0"
    TmTrue -> "true"
    TmFalse -> "false"
    TmIf t1 t2 t3 -> "(" ++ "if " ++ showTm' t1
      ++ "\n  then " ++ showTm' t2
      ++ "\n  else " ++ showTm' t3 ++ ")"
    TmUnit -> "unit"
    TmAscribe t1 ty -> "(" ++ showTm' t1 ++ " as " ++ showType ctx ty ++ ")"
    TmSeq t1 t2 -> "(" ++ showTm' t1 ++ ";" ++ showTm' t2 ++ ")"
    TmWildCard ty t2 -> "(" ++ "λ_:" ++ showType ("":ctx) ty ++ "." ++ showTm ("":ctx) t2 ++ ")"
    TmLet p t1 t2 -> let p' = map (fixName ctx) $ namesOfPattern p in
      "(let " ++ showPattern ctx p ++ " = " ++ showTm' t1 ++ "\n in " ++ showTm (p' ++ ctx) t2 ++ ")"
    TmRecord ts ->
      "{"
      ++ (intercalate ", "
        $ map (\((x, y), k) -> (if (show k /= x) then x ++ " = " else "") ++ showTm' y)
        $ zip ts [1..])
      ++ "}"
    TmProj t1 x -> "(" ++ showTm' t1 ++ "." ++ show x ++ ")"
    TmVariant x t1 ty -> let x' = fixName ctx x in
      "(" ++ "<" ++ x' ++ ", " ++ showTm' t1 ++ "> as " ++ showType ctx ty ++ ")"
    TmCase t1 ts ->
      "(case " ++ showTm' t1 ++ " of \n"
      ++ "    " ++ (intercalate "\n  | " (map (\(x, (y, z)) -> "<" ++ x ++ ", " ++ y ++ "> -> " ++ showTm (y:ctx) z) ts)) ++ ")"
    TmFix t1 -> "(fix " ++ showTm' t1 ++ ")"
    TmNil ty -> "nil[" ++ showType ctx ty ++ "]"
    TmCons ty t1 t2 -> "(cons[" ++ showType ctx ty ++ "] " ++ showTm' t1 ++ " " ++ showTm' t2 ++ ")"
    TmIsNil ty t1 -> "(isnil[" ++ showType ctx ty ++ "] " ++ showTm' t1 ++ ")"
    TmHead ty t1 -> "(head[" ++ showType ctx ty ++ "] " ++ showTm' t1 ++ ")"
    TmTail ty t1 -> "(tail[" ++ showType ctx ty ++ "] " ++ showTm' t1 ++ ")"
    TmErr e -> "#" ++ e ++ "#"
    TmTyAbs x t1 ->
      let x' = fixName' x
       in "(Λ" ++ x' ++ "." ++ showTm (x':ctx) t1 ++ ")"
    TmTyApp t1 ty -> "(" ++ showTm' t1 ++ " [" ++ showType ctx ty ++ "]" ++ ")"
    TmPack ty1 t1 ty2 -> "(" ++ "{*" ++ showType ctx ty1 ++ ", " ++ showTm' t1 ++ "} as " ++ showType ctx ty2 ++ ")"
    TmUnpack x1 x2 t1 t2 -> let x1' = fixName' x1; x2' = fixName' x2 in
      "(let {" ++ x1' ++ ", " ++ x2' ++ "} = " ++ showTm' t1 ++ "\n in " ++ showTm (x2':x1':ctx) t2 ++ ")"
  where showTm' = showTm ctx
        fixName' = fixName ctx
        tmVarErr l ctxLength = "#TmVar: bad context length: " ++ show l ++ "/=" ++ show ctxLength ++ "#"

getNameFromContext :: [Name] -> Index -> Name -> Name
getNameFromContext ctx ind x | ind >= 0 && ind < length ctx = ctx !! ind
                             | otherwise = x --"#TmVar: no name context for var#"

fixName :: [Name] -> Name -> Name
fixName ctx x | (length $ filter ((==) x) ctx) < 1 = x
              | otherwise = fixName ctx (x ++ "\'")

showFileInfo :: FileInfo -> String
showFileInfo (AlexPn p l c) =
  "\n" ++"Absolute Offset: " ++ show p ++ "\n"
  ++ "Line: " ++ show l ++ "\n"
  ++ "Column: " ++ show c

showType' :: Type -> String
showType' ty = removeOuterParens $ showType [] ty

showType :: [Name] -> Type -> String
showType ctx ty =
  case ty of
    TyNat -> "Nat"
    TyBool -> "Bool"
    TyUnit -> "Unit"
    TyArr ty1 ty2 -> "(" ++ showType ctx ty1 ++ " → " ++ showType ctx ty2 ++ ")"
    TyRecord tys ->
      "{"
      ++ (intercalate ", "
        $ map (\((x, y), k) -> (if (show k /= x) then x ++ " : " else "") ++ showType ctx y)
        $ zip tys [1..])
      ++ "}"
    TyVariant tys ->
      "<"
      ++ (intercalate ", "
        $ map (\((x, y), k) -> (if (show k /= x) then x ++ " : " else "") ++ showType ctx y)
        $ zip tys [1..])
      ++ ">"
    TyList ty -> "(List " ++ showType ctx ty ++ ")"
    TyUnknown -> "Unk"
    TyVar n -> "t" ++ show n
    TyErr e -> e
    TyVarF k l x -> let ctxLength = length ctx in
      if l == ctxLength
        then getNameFromContext ctx k x
        else tyVarFErr l ctxLength
    TyForAll x ty1 -> let x' = fixName ctx x in
      "(" ++ "∀" ++ x' ++ "." ++ showType (x':ctx) ty1 ++ ")"
    TyForSome x ty1 -> let x' = fixName ctx x in
      "{" ++ "∃" ++ x' ++ ", " ++ showType (x':ctx) ty1 ++ "}"
    _ -> "#Display showType: invalid type for showing: " ++ show ty ++ "#"
  where tyVarFErr l ctxLength = "#TyVarF: bad context length: " ++ show l ++ "/=" ++ show ctxLength ++ "#"

showPattern :: [Name] -> Pattern -> String
showPattern ctx p =
  case p of
    PVar x -> fixName ctx x
    PRecord ps ->
      "{"
      ++ (intercalate ", "
        $ map (\((x, y), k) -> (if (show k /= x) then x ++ " : " else "") ++ showPattern ctx y)
        $ zip ps [1..])
      ++ "}"

removeOuterParens :: String -> String
removeOuterParens xs
  | length xs >= 2 =
    let xs' = reverse $ getTail xs
     in
      if getHead xs == '(' && getHead xs' == ')'
        then reverse $ getTail xs'
        else xs
  | otherwise = xs
  where getHead = (\(x:_) -> x)
        getTail = (\(_:xs) -> xs)
