module Interface where

import Data.Text(Text)
import Data.Text.IO as IO

import Parser
import Eval
import Value
import Error

intepreter :: Text -> Either Error [Value]
intepreter src = fmap (map evalExpr) $ parseToAST src
