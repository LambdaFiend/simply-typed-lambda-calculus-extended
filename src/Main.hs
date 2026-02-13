module Main where

import Syntax
import Semantics
import Typing
import Display
import Lexer
import Parser
import Helper

import Data.Char
import Data.List
import System.IO
import System.Directory
import Control.Exception

type EnvVarName = String
type Environment = [(EnvVarName, TermNode)]
type CommandList = [String]

getHelp :: String
getHelp = (\s -> s ++ "\n") $ intercalate "\n" $
  "Command names (the first token of the command) are not case sensitive.\n"
    : "[:var, :v, :assign and :a assign a written term to <var_name>]\n"
    : ":v <var_name>\n"
    : "[:type, :ty and :t show the type of the term assiged to <var_name>]\n"
    : ":t <var_name>\n"
    : "[:eval, :ev and :e fully evaluate the term from <var_name>]\n"
    : ":e <var_name>\n"
    : "[:evaln, :evn and :en evaluate (<number_of_steps>) n-steps the term from <var_name>]\n"
    : ":en <number_of_steps> <var_name>\n"
    : "[:help, :h and :? display information regarding the commands]\n"
    : ":h\n"
    : "[:show, :sh and :s show the term assigned to <var_name>]\n"
    : ":s <var_name>\n"
    : "[:desugar, :desug, :des and :d are for cases where one wants to evaluate/typecheck using the desugared version]\n"
    : ":d <var_name1> <var_name2>\n"
    : "[Additionally, for command :var, :v, :assign and :a, a third argument may be added, "
    : "which is meant to be either :eval, :ev, :e, :evaln, :evn and :en, in which case "
    : "it evaluates from the current environment (given a <var_name2>) and then stores it into <var_name1>]\n"
    : ":v <var_name1> :ev <var_name2>\n"
    : ":v <var_name1> :evn <number_of_steps> <var_name2>\n"
    : "[It (the command :var, :v, :assign and :a) may also be used for reading a term from a file, with its path being specified at <file_name>]\n"
    : ":v <var_name> <file_path>\n"
    : "[:desugar, :desug, :des and :d desugar the term from <var_name1> and place it into <var_name2>]\n"
    : ":d <var_name1> <var_name2>\n"
    : "[:load and :l load terms from file at <file_path>, which are assigned inside the file as <var_name> := <expression>, and then loaded into the environment correspondingly]\n"
    : ":l <file_path>\n"
    : "[:v? and :vars show the first page (10 environment variables) of the environment, if a number is not specified]\n"
    : ":v?\n"
    : "[:v? and :vars will show the <number>'th page (containing 10 environment variables' names)]\n"
    : ":v? <number>\n"
    : "[:m, :mv and :move will store the contents of <var_name2> into <var_name1>]\n"
    : ":mv <var_name1> <var_name2>\n"
    : ":q and :quit close the REPL\n"
    : ":q\n"
    : []

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  comml <- handleCommHistFile "command_history.txt"
  evaluate (length comml)
  main' [] $ reverse $ lines comml

main' :: Environment -> CommandList -> IO ()
main' env comml = do
  putStr "stlce> "
  command <- readLine' comml
  putStrLn ""
  let commToks = (\comm -> case comm of (x:xs) -> map toLower x:xs; [] -> []) $ words command
  env' <- case (commToks) of
    [quit] | elem quit [":q", ":quit"] -> do
      return [("", TermNode noPos (TmErr "Quit."))]
    [move, name1, name2] | elem move [":move", ":mv", ":m"] -> do
      let x = lookup name2 env
      if x == Nothing
        then do
          putStrLn "Variable not in scope"
          return env
        else
          return ((name1, fromMaybe x):env)
    [vars] | elem vars [":v?", ":vars"] -> do
      putStrLn $ (intercalate "\n") $ (take 10) $ map fst env
      return env
    [vars, k] | and (map isDigit k) && elem vars [":v?", ":vars"] -> let k' = read k in do
      putStrLn $ (intercalate "\n") $ (drop (10 * (k' - 1))) $ (take (10 * k')) $ map fst env
      return env
    [load, file] | elem load [":load", ":l"]-> do
      txt <- readFile file
      let comms = simplyParseCommands' $ words txt
      asts <- getMultipleASTsFromTerms comms
      return $ asts ++ env
    [desug, name1, name2] | elem desug [":desugar", ":desug", ":des", ":d"] -> do
      let x = lookup name2 env
      if x == Nothing
        then do
          putStrLn "Variable not in scope"
          return env
        else do
          desugaredTerm <- getDesugaredTerm $ fromMaybe x
          return ((name1, desugaredTerm):env)
    [var, name, file] | elem var [":var", ":v", ":assign", ":a"] -> do
      txt <- getTxtFromFile file
      term <- getTermFromAST txt
      case term of
        Left e -> return env
        Right term' -> return ((name, term'):env)
    [var, name] | elem var [":var", ":v", ":assign", ":a"] -> do
      txt <- getTxtFromInput
      term <- getTermFromAST txt
      case term of
        Left e -> return env
        Right term' -> return ((name, term'):env)
    [ty, name] | elem ty [":type", ":ty", ":t"] -> do
      let x = lookup name env
      if x == Nothing
        then do
          putStrLn "Variable not in scope"
          return env
        else do
          printType $ fromMaybe x
          return env
    [ev, name] | elem ev [":eval", ":ev", ":e"] -> do
      let x = lookup name env
      if x == Nothing
        then do
          putStrLn "Variable not in scope"
          return env
        else do
          printEval $ fromMaybe x
          return env
    [ev, k, name] | and (map isDigit k) && elem ev [":eval", ":ev", ":e"] -> do
      let x = lookup name env
      if x == Nothing
        then do
          putStrLn "Variable not in scope"
          return env
        else do
          printEvalN (read k) $ fromMaybe x
          return env
    [help] | elem help [":help", ":h", ":?"] -> do
      putStrLn getHelp
      return env
    [s, name] | elem s [":show", ":sh", ":s"] -> do
      let x = lookup name env
      if x == Nothing
        then do
          putStrLn "Variable not in scope"
          return env
        else do
          printTerm $ fromMaybe x
          return env
    [var, name1, ev, name2]
      | elem var [":var", ":v", ":assign", ":a"]
        && elem ev [":eval", ":ev", ":e"] -> do
          let x = lookup name2 env
          if x == Nothing
            then do
              putStrLn "Variable not in scope"
              return env
            else do
              let term = fromMaybe x
              term' <- printEval term
              return ((name1, term'):env)
    [var, name1, ev, k, name2]
      | and (map isDigit k)
        && elem var [":var", ":v", ":assign", ":a"]
        && elem ev [":eval", ":ev", ":e"] -> do
          let x = lookup name2 env
          if x == Nothing
            then do
              putStrLn "Variable not in scope"
              return env
            else do
              let term = fromMaybe x
              term' <- printEvalN (read k) term
              return ((name1, term'):env)
    [] -> return env
    _ -> do
      putStrLn "Wrong command"
      return env
  if env' == [("", TermNode noPos (TmErr "Quit."))]
    then return ()
    else do
      let comml' = if (filter (/= ' ') command) == "" then comml else (command:comml)
      appendFile "command_history.txt" (command ++ "\n")
      main' env' comml'

simplyParseCommands' :: [String] -> [(String, String)]
simplyParseCommands' s = map (\(x, y) -> (x, intercalate " " y)) $ simplyParseCommands s

simplyParseCommands :: [String] -> [(String, [String])]
simplyParseCommands [] = []
simplyParseCommands (name:":=":xs) = let expr = getExpression xs in (name, fst expr):(simplyParseCommands $ snd expr)
  where getExpression :: [String] -> ([String], [String])
        getExpression [] = ([], [])
        getExpression s@(name:":=":xs) = ([], s)
        getExpression (x:xs) = let next = getExpression xs in (x:(fst next), snd next)

handleCommHistFile :: FilePath -> IO String
handleCommHistFile file = do
  fileExists <- doesFileExist file
  if fileExists
    then readFile file
    else do
      writeFile file ""
      return ""

readLine' :: CommandList -> IO String
readLine' comml = readLine ([], []) comml []

readLine :: (String, String) -> CommandList -> CommandList -> IO String
readLine (left, right) comml1 comml2 = do
  currChar <- getChar
  case currChar of
    '\n' -> return (left ++ right)
    '\DEL' -> case reverse left of
      (_:ls) -> do
        let left' = reverse ls
            lenLeft = length left
            lenRight = length right
        if right == ""
          then putStr $ "\ESC[1D \ESC[1D" ++ replicate lenRight ' '
          else do
            putStr "\r"
            putStr $ "stlce> " ++ replicate (lenLeft + lenRight) ' '
            putStr "\r"
            putStr $ "stlce> " ++ left' ++ right
            putStr $ "\ESC[" ++ show (length "stlce> " + lenLeft) ++ "G"
        readLine (left', right) comml1 comml2
      [] -> readLine (left, right) comml1 comml2
    '\ESC' -> do
      n1 <- getChar
      n2 <- getChar
      case n2 of
        'D' ->
          case reverse left of
            (l:ls) -> do
              putStr "\ESC[1D"
              readLine (reverse ls, l:right) comml1 comml2
            [] -> readLine (left, right) comml1 comml2
        'C' ->
          case right of
            (r:rs) -> do
              putChar r
              readLine (left ++ [r], rs) comml1 comml2
            [] -> readLine (left, right) comml1 comml2
        'A' -> do
          case comml1 of
            (c:cs) -> do
              putStr "\r"
              putStr $ "stlce> " ++ replicate (length (left ++ right)) ' '
              putStr "\r"
              putStr $ "stlce> " ++ c
              readLine (c, []) cs ((left++right):comml2)
            [] -> readLine (left, right) comml1 comml2
        'B' -> do
          case comml2 of
            (c:cs) -> do
              putStr "\r"
              putStr $ "stlce> " ++ replicate (length (left ++ right)) ' '
              putStr "\r"
              putStr $ "stlce> " ++ c
              readLine (c, []) ((left ++ right):comml1) cs
            [] -> readLine (left, right) comml1 comml2
    _ -> do
      putStr (currChar:right)
      putStr $ replicate (length right) '\b'
      readLine (left ++ [currChar], right) comml1 comml2

getTxtFromFile :: FilePath -> IO String
getTxtFromFile file = do
  txt <- readFile file
  return txt

getTxtFromInput :: IO String
getTxtFromInput = do
  txt <- readUntil' '\4'
  putStrLn ""
  return txt

updateNextLines :: [String] -> Int -> IO ()
updateNextLines [x] n = do
  putStr "\r"
  putStr $ replicate (n + 1) ' '
  putStr "\r"
  putStr x
  putStr "\r"
updateNextLines (x:xs) n = do
  putStr "\r"
  putStr $ replicate (n + 1) ' '
  putStr "\r"
  putStr x
  putStr "\r"
  putStr $ "\ESC[1A"
  updateNextLines xs $ length x

readUntil' :: Char -> IO String
readUntil' finalChar = do
  (leftRight, prevLines, nextLines) <- readUntil finalChar ([], []) [] [] 0 0
  return $ concat (prevLines ++ leftRight ++ nextLines)

readUntil :: Char -> (String, String) -> [String] -> [String] -> Int -> Int -> IO ([String], [String], [String])
readUntil finalChar (left, right) prevLines nextLines currH maxH = do
  currChar <- getChar
  case currChar of
    '\n' -> do
      let lenNextLines = length nextLines
      if (lenNextLines > 0)
        then putStr $ "\ESC[" ++ show lenNextLines ++ "B" ++ ""
        else return ()
      putStr "\n"
      updateNextLines (reverse (right:nextLines)) 0
      putStr "\ESC[1A"
      putStr "\r"
      putStr $ replicate (length left + length right) ' '
      putStr "\r"
      putStr left
      putStr "\r"
      putStr "\ESC[1B"
      readUntil finalChar ([], right) (left:prevLines) nextLines (currH + 1) (maxH + 1)
    '\DEL' -> case reverse left of
      (_:ls) -> do
        let left' = reverse ls
            lenLeft = length left
            lenRight = length right
        if right == ""
          then putStr $ "\ESC[1D \ESC[1D" ++ replicate lenRight ' '
          else do
            putStr "\r"
            putStr $ "stlce> " ++ replicate (lenLeft + lenRight) ' '
            putStr "\r"
            putStr $ "stlce> " ++ left' ++ right
            putStr $ "\ESC[" ++ show (length "stlce> " + lenLeft) ++ "G"
        readUntil finalChar (left', right) prevLines nextLines currH maxH
      [] -> readUntil finalChar (left, right) prevLines nextLines currH maxH
    '\ESC' -> do
      n1 <- getChar
      n2 <- getChar
      case n2 of
        'D' -> do
          putStr "\r"
          putStr $ replicate (length right + length left + 10) ' '
          putStr "\r"
          putStr $ left ++ right
          putStr $ replicate (length right) '\b'
          case reverse left of
            (l:ls) -> do
              putStr "\ESC[1D"
              readUntil finalChar (reverse ls, l:right) prevLines nextLines currH maxH
            [] ->
              if currH > 0
                then do
                  let left' = getHead prevLines
                  putStr "\ESC[1A"
                  putStr "\r"
                  putStr $ left'
                  readUntil finalChar (left', []) (getTail prevLines) ((left ++ right):nextLines) (currH - 1) maxH
                else readUntil finalChar (left, right) prevLines nextLines currH maxH
        'C' -> do
          putStr "\r"
          putStr $ replicate (length right + length left + 10) ' '
          putStr "\r"
          putStr $ left ++ right
          putStr $ replicate (length right) '\b'
          case right of
            (r:rs) -> do
              putChar r
              readUntil finalChar (left ++ [r], rs) prevLines nextLines currH maxH
            [] ->
              if currH < maxH
                then do
                  let right' = getHead nextLines
                  putStr "\ESC[1B"
                  putStr "\r"
                  putStr $ right'
                  putStr "\r"
                  readUntil finalChar ([], right') ((left ++ right):prevLines) (getTail nextLines) (currH + 1) maxH
                else readUntil finalChar (left, right) prevLines nextLines currH maxH
        'A' -> do
          if currH == 0
            then do
              putStr "\r"
              readUntil finalChar ([], left ++ right) prevLines nextLines currH maxH
            else do
              putStr "\ESC[1A"
              let left' = take (length left) $ getHead prevLines
                  right' = drop (length left) $ getHead prevLines
              putStr "\r"
              putStr $ replicate (length right' + length left' + 10) ' '
              putStr "\r"
              putStr $ left' ++ right'
              putStr $ "\ESC[" ++ show (length left' + 1) ++ "G"
              readUntil finalChar (left', right') (getTail prevLines) ((left ++ right):nextLines) (currH - 1) maxH
        'B' -> do
          if currH == maxH
            then do
              putStr "\r"
              putStr $ left ++ right
              readUntil finalChar (left ++ right, []) prevLines nextLines currH maxH
            else do
              putStr "\ESC[1B"
              let left' = take (length left) $ getHead nextLines
                  right' = drop (length left) $ getHead nextLines
              putStr "\r"
              putStr $ replicate (length right' + length left' + 10) ' '
              putStr "\r"
              putStr $ left' ++ right'
              putStr $ "\ESC[" ++ show (length left' + 1) ++ "G"
              readUntil finalChar (left', right') ((left ++ right):prevLines) (getTail nextLines) (currH + 1) maxH
    _ -> do
      if currChar == finalChar
        then return $ ([left ++ right], prevLines, nextLines)
        else do
          putStr (currChar:right)
          putStr $ replicate (length right) '\b'
          readUntil finalChar (left ++ [currChar], right) prevLines nextLines currH maxH
  where getHead = \ls -> case ls of [] -> []; (x:xs) -> x
        getTail = \ls -> case ls of [] -> []; (x:xs) -> xs

getTokens :: String -> IO [Token]
getTokens txt = return $ alexScanTokens txt

getAST :: String -> IO (Either String TermNode)
getAST txt = do
  tok <- getTokens txt
  let tokErr = filter (\x -> case x of Token _ (ERR e) -> True; _ -> False) tok
  case tokErr of
    (x:xs) -> return $ Left $ (\(Token fi (ERR e)) -> e ++ showFileInfo fi) $ x
    [] -> return $ parser tok

getTermFromAST :: String -> IO (Either String TermNode)
getTermFromAST txt = do
  ast <- getAST txt
  case ast of
    Left e -> do
      putStrLn e
      return $ Left ""
    Right ast' -> return $ Right $ genIndex' ast'

getMultipleASTsFromTerms :: [(String, String)] -> IO [(String, TermNode)]
getMultipleASTsFromTerms [] = return []
getMultipleASTsFromTerms (x:xs) = do
  term <- getTermFromAST $ snd x
  next <- getMultipleASTsFromTerms xs
  case term of
    Left e -> do
      putStrLn $ "for environment variable: " ++ fst x
      return next
    Right term' -> return ((fst x, term'):next)

getDesugaredTerm :: TermNode -> IO TermNode
getDesugaredTerm ast = return $ desugarTm' ast

printEval :: TermNode -> IO TermNode
printEval ast = do
  ast' <- if isVal ast
    then do
      putStrLn "The given term is already a value"
      return ast
    else do
      let ast' = eval' ast
          errs = findTermErrors' $ snd ast'
      if errs /= []
        then do
          putStrLn errs
          return $ TermNode noPos $ TmErr ""
        else do
          putStrLn $ "The given term evaluated a total of " ++ (show $ fst ast') ++ " times: "
          putStrLn $ findDisplayErrors' $ showTm' $ snd ast'
          return $ snd ast'
  return $ ast'

printEvalN :: Counter -> TermNode -> IO TermNode
printEvalN n ast = do
  ast' <-
    if isVal ast
      then do
        putStrLn "The given term is already a value"
        return ast
      else do
        let ast' = evalN n ast
            errs = findTermErrors' $ snd ast'
        if errs /= []
          then do
            putStrLn errs
            return $ TermNode noPos $ TmErr ""
          else do
            putStrLn $ "The given term evaluated a total of " ++ (show $ (n - fst ast')) ++ " times: "
            putStrLn $ findDisplayErrors' $ showTm' $ snd ast'
            return $ snd ast'
  return $ ast'

printType :: TermNode -> IO ()
printType ast = do
  let typingMethod = getTypingMethod ast
  case typingMethod of
    TypingError e -> putStrLn e
    Check -> do
      let ty = typeOf' ast
          errs = findTypeErrors' ty
      if errs /= []
        then putStrLn errs
        else do
          putStrLn "The term's type was checked"
          putStrLn "Its type:"
          putStrLn $ showType' ty
    _ -> do
      case inferT' ast of
        Left e -> putStrLn e
        Right ty -> do
          putStrLn "The term's type was inferred"
          putStrLn "The required context:"
          putStrLn $ show $ map (\(x, y) -> (x, showType y)) $ fst ty
          putStrLn "Its principal type:"
          putStrLn $ showType $ snd ty

printTerm :: TermNode -> IO ()
printTerm t = do
  putStrLn $ findDisplayErrors' $ showTm' t
