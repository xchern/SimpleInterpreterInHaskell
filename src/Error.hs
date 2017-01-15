module Error where

data Error = ParseError String
           | TypeError String

instance Show Error where
  show (ParseError msg) = "ParseError: " ++ msg
  show (TypeError msg) = "TypeError: " ++ msg
