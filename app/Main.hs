module Main where

import Control.Applicative
import Control.Monad.State
import System.Environment
import System.IO
import qualified Data.Text as Text
import qualified Data.Text.IO as IO

import Interface

data Action = Interprete | Parse
  deriving Show

data Out = StdOut | File FilePath
  deriving Show

data Option = Option { inPath :: String
                     , outPath :: Out
                     , action :: Action
                     }
            | REPL
            deriving Show

type Parser a = StateT [String] Maybe a

parseFlag :: String -> Parser String
parseFlag f = do
    args <- get
    case args of
        [] -> empty
        (arg : args')
            | arg == f -> do
                put args'
                return f
            | otherwise -> empty
            
parseField :: String -> Parser String
parseField f = do
    parseFlag f
    args <- get
    case args of
        [] -> empty
        (arg : args') -> do
            put args'
            return arg
            
parseInPathI :: Parser String
parseInPathI = parseField "-i"

parseInPathP :: Parser String
parseInPathP = parseField "-t"

parseOutPath :: Parser String
parseOutPath = parseField "-o"

parseOption :: Parser Option
parseOption = do
  parseFlag "-repl"
  return REPL
  <|> do
  i <- parseInPathI
  o <- parseOutPath
  return (Option i (File o) Interprete)
  <|> do
  i <- parseInPathP
  o <- parseOutPath
  return (Option i (File o) Parse)
  <|> do
  i <- parseInPathI
  return (Option i StdOut Interprete)
  <|> do
  i <- parseInPathP
  return (Option i StdOut Parse)

main :: IO ()
main = do
    args <- getArgs
    case runStateT parseOption args of
      Just (REPL, _) -> repl
      Just (Option pIn o a, _) -> let
        instream = IO.readFile pIn
        outstream = case o of
                      StdOut -> putStr
                      File po -> writeFile po in
        case a of
          Interprete -> (instream >>= (outstream . interprete))
          Parse -> (instream >>= (outstream . parse))
      Nothing -> do
        putStrLn "Ill Arguments."
        putStrLn "use with [-repl] [-i SRC-FILE] [-t SRC-FILE] [-o OUTPUT-FILE]"

repl :: IO ()
repl = do
  putStr ">> " -- IO monad bind :P
  hFlush stdout
  line <- getLine
  putStr $ interprete (Text.pack line)
  repl
