module Display where

import Syntax
import Lexer
import Data.List

showTm' :: TermNode -> String
showTm' t = showTm [] t

showTm :: [Name] -> TermNode -> String
showTm ctx t = let tm = getTm t in
  case tm of
    TmVar k l -> let ctxLength = length ctx in
      if (l == ctxLength)
        then getNameFromContext ctx k
        else error $ tmVarErr l ctxLength
    TmAbs x ty t1 ->
      let x' = fixName' x
       in "(" ++ "λ" ++ x' ++ ":" ++ showType ty ++ "." ++ showTm (x':ctx) t1 ++ ")"
    TmApp t1 t2 -> "(" ++ showTm' t1 ++ " " ++ showTm' t2 ++ ")"
    TmSucc t1 -> "(" ++ "succ " ++ showTm' t1 ++ ")"
    TmPred t1 -> "(" ++ "pred " ++ showTm' t1 ++ ")"
    TmIsZero t1 -> "(" ++ "iszero " ++ showTm' t1 ++ ")"
    TmZero -> "0"
    TmTrue -> "true"
    TmFalse -> "false"
    TmIf t1 t2 t3 -> "(" ++ "if " ++ showTm' t1
      ++ " then " ++ showTm' t2
      ++ " else " ++ showTm' t3 ++ ")"
    TmUnit -> "unit"
    TmAscribe t1 ty -> "(" ++ showTm' t1 ++ " as " ++ showType ty ++ ")"
    TmSeq t1 t2 -> "(" ++ showTm' t1 ++ ";" ++ showTm' t2 ++ ")"
    TmWildCard ty t2 -> "(" ++ "λ_:" ++ showType ty ++ "." ++ showTm ("":ctx) t2 ++ ")"
    TmLet p t1 t2 -> let p' = map (fixName ctx) $ namesOfPattern p in
      "(let " ++ showPattern ctx p ++ " = " ++ showTm' t1 ++ " in " ++ showTm (p' ++ ctx) t2 ++ ")"
    TmRecord ts ->
      "{"
      ++ (intercalate ", "
        $ map (\((x, y), k) -> (if (show k /= x) then x ++ " = " else "") ++ showTm' y)
        $ zip ts [1..])
      ++ "}"
    TmProj t1 x -> "(" ++ showTm' t1 ++ "." ++ show x ++ ")"
    TmVariant x t1 ty -> let x' = fixName ctx x in
      "(" ++ "<" ++ x' ++ ", " ++ showTm' t1 ++ "> as " ++ showType ty ++ ")"
    TmCase t1 ts ->
      "(case " ++ showTm' t1 ++ " of \n"
      ++ "    " ++ (intercalate "\n  | " (map (\(x, (y, z)) -> "<" ++ x ++ ", " ++ y ++ "> -> " ++ showTm (y:ctx) z) ts)) ++ ")"
    TmFix t1 -> "(fix " ++ showTm' t1 ++ ")"
    TmNil ty -> "nil[" ++ showType ty ++ "]"
    TmCons ty t1 t2 -> "(cons[" ++ showType ty ++ "] " ++ showTm' t1 ++ " " ++ showTm' t2 ++ ")"
    TmIsNil ty t1 -> "(isnil[" ++ showType ty ++ "] " ++ showTm' t1 ++ ")"
    TmHead ty t1 -> "(head[" ++ showType ty ++ "] " ++ showTm' t1 ++ ")"
    TmTail ty t1 -> "(tail[" ++ showType ty ++ "] " ++ showTm' t1 ++ ")"
  where showTm' = showTm ctx
        fixName' = fixName ctx
        tmVarErr l ctxLength = "TmVar: bad context length: " ++ show l ++ "/=" ++ show ctxLength

getNameFromContext :: [Name] -> Index -> Name
getNameFromContext ctx ind | ind < length ctx = ctx !! ind
                           | otherwise = error "TmVar: no name context for var"

fixName :: [Name] -> Name -> Name
fixName ctx x | (length $ filter ((==) x) ctx) < 1 = x
              | otherwise = fixName ctx (x ++ "\'")

showFileInfo :: FileInfo -> String
showFileInfo (AlexPn p l c) =
  "\n" ++"Absolute Offset: " ++ show p ++ "\n"
  ++ "Line: " ++ show l ++ "\n"
  ++ "Column: " ++ show c ++ "\n"

showType :: Type -> String
showType ty =
  case ty of
    TyNat -> "Nat"
    TyBool -> "Bool"
    TyUnit -> "Unit"
    TyArr ty1 ty2 -> "(" ++ showType ty1 ++ "->" ++ showType ty2 ++ ")"
    TyRecord tys ->
      "{"
      ++ (intercalate ", "
        $ map (\((x, y), k) -> (if (show k /= x) then x ++ " : " else "") ++ showType y)
        $ zip tys [1..])
      ++ "}"
    TyVariant tys ->
      "<"
      ++ (intercalate ", "
        $ map (\((x, y), k) -> (if (show k /= x) then x ++ " : " else "") ++ showType y)
        $ zip tys [1..])
      ++ ">"
    TyList ty -> "(List " ++ showType ty ++ ")"
    TyUnknown -> "Unk"
    TyVar n -> "α" ++ show n

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
