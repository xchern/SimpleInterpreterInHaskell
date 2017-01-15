module Error where

data Error = ParseError String
           | TypeError String
