module Interface where

import Data.Text(Text)
import Data.Either

import Parser
import Eval
import Value
import Error

parse :: Text -> String
parse = showASTs . parseToAST

interprete :: Text -> String
interprete src = case parseToAST src of
  Left e -> show e
  Right asts -> let
    (errors, values) = partitionEithers $ map evalExpr asts in
    if (null errors)
    then unlines $ map show values
    else unlines $ map show errors

showASTs :: Either Error [Expr] -> String
showASTs (Left err) = show err
showASTs (Right expr) = unlines $ map show expr

showValues :: Either Error [Value] -> String
showValues (Left err) = show err
showValues (Right vs) = unlines $ map show vs
