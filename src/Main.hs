module Main where

import Syntax
import Semantics
import Typing
import Display
import Lexer
import Parser
import Helper

main :: IO ()
main = do
  txt <- readFile "_Testes/teste.txt"
  let tok = alexScanTokens txt
  putStrLn $ show tok
  putStrLn ""
  let ast = desugarTm' $ genIndex' $ parser tok
  putStrLn $ show $ ast
  putStrLn ""
  let str = showTm' $ ast
  putStrLn str
  putStrLn ""
  let typ = typeOf' ast
  putStrLn $ showType typ
  putStrLn ""
  let new = showTm' $ eval1 ast
  putStrLn new
  putStrLn ""
  let new = showTm' $ eval ast
  putStrLn new
  putStrLn ""

