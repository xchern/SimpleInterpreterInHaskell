module Error where

data Error = ParserError String
           | TypeError String
           -- Refering to undefined atom
           | ReferenceError String
