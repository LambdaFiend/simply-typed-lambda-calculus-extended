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
import System.Console.ANSI

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
    : ("[Additionally, for command :var, :v, :assign and :a, a third argument may be added, "
      ++ "which is meant to be either :eval, :ev, :e, :evaln, :evn and :en, in which case "
      ++ "it evaluates from the current environment (given a <var_name2>) and then stores it into <var_name1>]\n")
    : ":v <var_name1> :ev <var_name2>\n"
    : ":v <var_name1> :evn <number_of_steps> <var_name2>\n"
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
    : "[:q and :quit close the REPL]\n"
    : ":q\n"
    : "[:te, :tenv and :testenv attempt to type all variables in the environment]\n"
    : ":testenv\n"
    : "[:ee, :eenv and :evalenv attempt to evaluate all variables in the environment]\n"
    : ":evalenv\n"
    : "[:c, :ce, :cenv, :clear and :clearenv clear the environment, which means there will be no variables accessible until new ones are added]\n"
    : ":c\n"
    : "[:de, :denv, :desenv, :desugenv and :desugarenv desugar all variables in the environment]\n"
    : ":de\n"
    : "[:av? and :allvars show all variables in the environment]\n"
    : ":av?\n"
    : "[:showenv, :showe, :senv and :se]\n"
    : ":se\n"
    : "[The commands for showing, typing and evaluating the environment can also be used for environment pages, as follows]\n"
    : ":se <page_number>"
    : ":te <page_number>"
    : ":ee <page_number>"
    : "[Page numbers start at 1]"
    : []

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
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
    [] -> return env
    [tenv, k] | and (map isDigit k) && elem tenv [":te", ":tenv", ":typeenv"] -> do
      let k' = read k
          env' = drop ((k' - 1) * 10) $ take (k' * 10) env
      errs <- typeEnvironment env'
      if errs /= [] then putStrLn "" else return ()
      putStrLn $ intercalate "\n" errs
      if errs /= [] then putStrLn "" else return ()
      putStr $ "There was a total of "
      setSGR [SetColor Foreground Vivid Red]
      putStr $ show (length errs)
      setSGR [Reset]
      putStrLn $ " type errors " ++ "on page " ++ k
      return env
    [eenv, k] | and (map isDigit k) && elem eenv [":ee", ":eenv", ":evalenv"] -> do
      let k' = read k
          env' = drop ((k' - 1) * 10) $ take (k' * 10) env
      errs <- evalEnvironment env'
      if errs /= [] then putStrLn "" else return ()
      putStrLn $ intercalate "\n" errs
      if errs /= [] then putStrLn "" else return ()
      putStr $ "There was a total of "
      setSGR [SetColor Foreground Vivid Red]
      putStr $ show (length errs)
      setSGR [Reset]
      putStrLn $ " evaluation errors " ++ "on page " ++ k
      return env
    [senv, k] | and (map isDigit k) && elem senv [":showenv", ":showe", ":senv", ":se"] -> do
      let k' = read k
          env' = drop ((k' - 1) * 10) $ take (k' * 10) env
      errs <- showEnvironment env'
      if errs /= [] then putStrLn "" else return ()
      putStrLn $ intercalate "\n" errs
      if errs /= [] then putStrLn "" else return ()
      putStr $ "There was a total of "
      setSGR [SetColor Foreground Vivid Red]
      putStr $ show (length errs)
      setSGR [Reset]
      putStrLn $ " display errors " ++ "on page " ++ k
      return env
    [senv] | elem senv [":showenv", ":showe", ":senv", ":se"] -> do
      errs <- showEnvironment env
      if errs /= [] then putStrLn "" else return ()
      putStrLn $ intercalate "\n" errs
      if errs /= [] then putStrLn "" else return ()
      putStr $ "There was a total of "
      setSGR [SetColor Foreground Vivid Red]
      putStr $ show (length errs)
      setSGR [Reset]
      putStrLn " display errors in the environment"
      return env
    [denv] | elem denv [":desugarenv", ":desugenv", ":desenv", ":denv", ":de"] -> do
      env' <- desugarEnvironment env
      setSGR [SetColor Foreground Vivid Green]
      putStrLn "Environment desugared"
      setSGR [Reset]
      return env'
    [cenv] | elem cenv [":clearenv", ":clear", ":cenv", ":ce", ":c"] -> do
      setSGR [SetColor Foreground Vivid Green]
      putStrLn "Environment cleared"
      setSGR [Reset]
      return []
    [tenv] | elem tenv [":te", ":tenv", ":typeenv"] -> do
      errs <- typeEnvironment env
      if errs /= [] then putStrLn "" else return ()
      putStrLn $ intercalate "\n" errs
      if errs /= [] then putStrLn "" else return ()
      putStr $ "There was a total of "
      setSGR [SetColor Foreground Vivid Red]
      putStr $ show (length errs)
      setSGR [Reset]
      putStrLn " type errors in the environment"
      return env
    [eenv] | elem eenv [":ee", ":eenv", ":evalenv"] -> do
      errs <- evalEnvironment env
      if errs /= [] then putStrLn "" else return ()
      putStrLn $ intercalate "\n" errs
      if errs /= [] then putStrLn "" else return ()
      putStr $ "There was a total of "
      setSGR [SetColor Foreground Vivid Red]
      putStr $ show (length errs)
      setSGR [Reset]
      putStrLn " evaluation errors in the environment"
      return env
    [quit] | elem quit [":q", ":quit"] -> do
      setSGR [SetColor Foreground Vivid Yellow]
      putStrLn "Leaving STLCE."
      setSGR [Reset]
      return [("", TermNode noPos (TmErr "Quit."))]
    [move, name1, name2] | elem move [":move", ":mv", ":m"] -> do
      let x = lookup name2 env
      if x == Nothing
        then do
          setSGR [SetColor Foreground Vivid Red]
          putStrLn "Variable not in scope"
          setSGR [Reset]
          return env
        else do
          let env' = deleteByFstEnv name1 env
          setSGR [SetColor Foreground Vivid Green]
          putStrLn "Variable moved"
          setSGR [Reset]
          return ((name1, fromMaybe x):env')
    [avars] | elem avars [":av?", ":allvars"] -> do
      setSGR [SetColor Foreground Vivid Yellow]
      putStrLn $ "All variables in the environment are:"
      setSGR [Reset]
      putStrLn $ (intercalate "\n") $ map fst env
      return env
    [vars] | elem vars [":v?", ":vars"] -> do
      setSGR [SetColor Foreground Vivid Yellow]
      putStrLn $ "The variables for page number 1 (10 vars per page) are:"
      setSGR [Reset]
      putStrLn $ (intercalate "\n") $ (take 10) $ map fst env
      return env
    [vars, k] | and (map isDigit k) && elem vars [":v?", ":vars"] -> let k' = read k in do
      setSGR [SetColor Foreground Vivid Yellow]
      putStrLn $ "The variables for page number " ++ k ++ " (10 vars per page) are:"
      setSGR [Reset]
      putStrLn $ (intercalate "\n") $ (drop (10 * (k' - 1))) $ (take (10 * k')) $ map fst env
      return env
    [load, file] | elem load [":load", ":l"]-> do
      fileExists <- doesFileExist ("programs/" ++ file)
      if fileExists
        then do
          txt <- readFile ("programs/" ++ file)
          let comms = simplyParseCommands' $ words txt
          asts <- getMultipleASTsFromTerms comms
          let env' = foldr deleteByFstEnv env (map fst asts)
          setSGR [SetColor Foreground Vivid Green]
          putStrLn $ "Loaded a total of " ++ (show $ length comms) ++ " environment variables"
          setSGR [Reset]
          return $ asts ++ env'
        else do
          setSGR [SetColor Foreground Vivid Red]
          putStrLn "Path leads to nowhere"
          setSGR [Reset]
          return env
    [desug, name1, name2] | elem desug [":desugar", ":desug", ":des", ":d"] -> do
      let x = lookup name2 env
      if x == Nothing
        then do
          setSGR [SetColor Foreground Vivid Red]
          putStrLn "Variable not in scope"
          setSGR [Reset]
          return env
        else do
          desugaredTerm <- getDesugaredTerm $ fromMaybe x
          let env' = deleteByFstEnv name1 env
          setSGR [SetColor Foreground Vivid Green]
          putStrLn "Variable desugared"
          setSGR [Reset]
          return ((name1, desugaredTerm):env')
    [var, name] | elem var [":var", ":v", ":assign", ":a"] -> do
      txt <- getTxtFromInput
      term <- getTermFromAST txt
      case term of
        Left e -> return env
        Right term' -> do
          let env' = deleteByFstEnv name env
          setSGR [SetColor Foreground Vivid Green]
          putStrLn "Variable assigned"
          setSGR [Reset]
          return ((name, term'):env')
    [ty, name] | elem ty [":type", ":ty", ":t"] -> do
      let x = lookup name env
      if x == Nothing
        then do
          setSGR [SetColor Foreground Vivid Red]
          putStrLn "Variable not in scope"
          setSGR [Reset]
          return env
        else do
          printType $ fromMaybe x
          return env
    [ev, name] | elem ev [":eval", ":ev", ":e"] -> do
      let x = lookup name env
      if x == Nothing
        then do
          setSGR [SetColor Foreground Vivid Red]
          putStrLn "Variable not in scope"
          setSGR [Reset]
          return env
        else do
          printEval $ fromMaybe x
          return env
    [ev, k, name] | and (map isDigit k) && elem ev [":eval", ":ev", ":e"] -> do
      let x = lookup name env
      if x == Nothing
        then do
          setSGR [SetColor Foreground Vivid Red]
          putStrLn "Variable not in scope"
          setSGR [Reset]
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
          setSGR [SetColor Foreground Vivid Red]
          putStrLn "Variable not in scope"
          setSGR [Reset]
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
              setSGR [SetColor Foreground Vivid Red]
              putStrLn "Variable not in scope"
              setSGR [Reset]
              return env
            else do
              let term = fromMaybe x
              term' <- printEval term
              let env' = deleteByFstEnv name1 env
              return ((name1, term'):env')
    [var, name1, ev, k, name2]
      | and (map isDigit k)
        && elem var [":var", ":v", ":assign", ":a"]
        && elem ev [":eval", ":ev", ":e"] -> do
          let x = lookup name2 env
          if x == Nothing
            then do
              setSGR [SetColor Foreground Vivid Red]
              putStrLn "Variable not in scope"
              setSGR [Reset]
              return env
            else do
              let term = fromMaybe x
              term' <- printEvalN (read k) term
              let env' = deleteByFstEnv name1 env
              return ((name1, term'):env')
    ((':':_):_) -> do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn "Unknown command"
      setSGR [Reset]
      return env
    ws -> do
      term <- getTermFromAST command
      case term of
        Left e -> return env
        Right term' -> do
          setSGR [SetColor Foreground Vivid Yellow]
          putStrLn "Typing:"
          setSGR [Reset]
          printType term'
          putStrLn ""
          setSGR [SetColor Foreground Vivid Yellow]
          putStrLn "Evaluating:"
          setSGR [Reset]
          printEval term'
          return env
  if env' == [("", TermNode noPos (TmErr "Quit."))]
    then return ()
    else do
      let comml' = if (filter (/= ' ') command) == "" then comml else (command:comml)
      appendFile "command_history.txt" (command ++ "\n")
      main' env' comml'

deleteByFstEnv :: String -> Environment -> Environment
deleteByFstEnv x xs =
  case break (\(x', _) -> x == x') xs of
    (prev, _:next) -> prev ++ next
    _              -> xs

desugarEnvironment :: Environment -> IO Environment
desugarEnvironment [] = return []
desugarEnvironment (e:env) = do
  desugaredTerm <- getDesugaredTerm $ snd e
  desugaredEnvironment <- desugarEnvironment env
  return ((fst e, desugaredTerm):desugaredEnvironment)

showEnvironment :: Environment -> IO [String]
showEnvironment [] = return []
showEnvironment (e:env) = do
  setSGR [SetColor Foreground Vivid Green]
  putStrLn $ fst e ++ ":"
  setSGR [Reset]
  first <- printTerm $ snd e
  case first of
    Left _ -> do
      rest <- showEnvironment env
      return ((fst e ++ " had a display error"):rest)
    Right _ -> do
      rest <- showEnvironment env
      return rest

typeEnvironment :: Environment -> IO [String]
typeEnvironment [] = return []
typeEnvironment (e:env) = do
  setSGR [SetColor Foreground Vivid Green]
  putStrLn $ fst e ++ ":"
  setSGR [Reset]
  first <- printType $ snd e
  case first of
    Left _ -> do
      rest <- typeEnvironment env
      return ((fst e ++ " had a type error"):rest)
    Right _ -> do
      rest <- typeEnvironment env
      return rest

evalEnvironment :: Environment -> IO [String]
evalEnvironment [] = return []
evalEnvironment (e:env) = do
  setSGR [SetColor Foreground Vivid Green]
  putStrLn $ fst e ++ ":"
  setSGR [Reset]
  first <- printEval $ snd e
  case first of
    TermNode noPos (TmErr "") -> do
      rest <- evalEnvironment env
      return ((fst e ++ " had an evaluation error"):rest)
    _ -> do
      rest <- evalEnvironment env
      return rest

simplyParseCommands' :: [String] -> [(String, String)]
simplyParseCommands' s = map (\(x, y) -> (x, intercalate " " y)) $ simplyParseCommands s

simplyParseCommands :: [String] -> [(String, [String])]
simplyParseCommands [] = []
simplyParseCommands (name:":=":xs) = let expr = getExpression xs in (name, fst expr):(simplyParseCommands $ snd expr)
  where getExpression :: [String] -> ([String], [String])
        getExpression [] = ([], [])
        getExpression s@(name:":=":xs) = ([], s)
        getExpression (x:xs) = let next = getExpression xs in (x:(fst next), snd next)
simplyParseCommands xs = []

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
        key | key /= '\^C' -> readLine (left, right) comml1 comml2
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
  putStrLn "Press Ctrl-D to submit the program"
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
        key | key /= '\^C' -> readUntil finalChar (left, right) prevLines nextLines currH maxH
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
      setSGR [SetColor Foreground Vivid Red]
      putStrLn e
      setSGR [Reset]
      return $ Left ""
    Right ast' -> return $ Right $ genIndex' ast'

getMultipleASTsFromTerms :: [(String, String)] -> IO [(String, TermNode)]
getMultipleASTsFromTerms [] = return []
getMultipleASTsFromTerms (x:xs) = do
  term <- getTermFromAST $ snd x
  next <- getMultipleASTsFromTerms xs
  case term of
    Left e -> do
      putStr "for environment variable: "
      setSGR [SetColor Foreground Vivid Red]
      putStrLn $ fst x
      setSGR [Reset]
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
          setSGR [SetColor Foreground Vivid Red]
          putStrLn "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
          setSGR [Reset]
          return $ TermNode noPos $ TmErr ""
        else do
          setSGR [SetColor Foreground Vivid Green]
          putStrLn $ "The given term evaluated a total of " ++ (show $ fst ast') ++ " times: "
          setSGR [Reset]
          printTerm $ snd ast'
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
            setSGR [SetColor Foreground Vivid Red]
            putStrLn "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
            setSGR [Reset]
            return $ TermNode noPos $ TmErr ""
          else do
            setSGR [SetColor Foreground Vivid Green]
            putStrLn $ "The given term evaluated a total of " ++ (show $ (n - fst ast')) ++ " times: "
            setSGR [Reset]
            printTerm $ snd ast'
            return $ snd ast'
  return $ ast'

printType :: TermNode -> IO (Either String String)
printType ast = do
  let typingMethod = getTypingMethod ast
  case typingMethod of
    TypingError e -> do
      putStrLn e
      return $ Left ""
    Check -> do
      let ty = typeOf' ast
          errs = findTypeErrors' ty
      if errs /= []
        then do
          putStrLn errs
          setSGR [SetColor Foreground Vivid Red]
          putStrLn "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
          setSGR [Reset]
          return $ Left ""
        else do
          setSGR [SetColor Foreground Vivid Green]
          putStrLn "The term's type was checked"
          setSGR [Reset]
          setSGR [SetColor Foreground Vivid Blue]
          putStrLn "Its type:"
          setSGR [Reset]
          putStrLn $ showType' ty
          return $ Right ""
    _ -> do
      case inferT' ast of
        Left e -> do
          putStrLn e
          setSGR [SetColor Foreground Vivid Red]
          putStrLn "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
          setSGR [Reset]
          return $ Left ""
        Right ty -> do
          setSGR [SetColor Foreground Vivid Green]
          putStrLn "The term's type was inferred"
          setSGR [Reset]
          setSGR [SetColor Foreground Vivid Blue]
          putStrLn "The required context:"
          setSGR [Reset]
          putStrLn $ show $ map (\(x, y) -> (x, showType y)) $ fst ty
          setSGR [SetColor Foreground Vivid Blue]
          putStrLn "Its principal type:"
          setSGR [Reset]
          putStrLn $ showType $ snd ty
          return $ Right ""

printTerm :: TermNode -> IO (Either String String)
printTerm t = do
  let display = findDisplayErrors' $ showTm' t
  case display of
    Left e -> do
      putStrLn e
      setSGR [SetColor Foreground Vivid Red]
      putStrLn "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
      setSGR [Reset]
      return $ Left ""
    Right s -> do
      putStrLn s
      return $ Right ""
