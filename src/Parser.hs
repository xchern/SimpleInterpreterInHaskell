{-# LANGUAGE OverloadedStrings #-}
module Parser ( parseToAST
              )
where

import Data.Functor
import Control.Applicative
import Data.Attoparsec.Text
import Data.Text(Text)

import Value
import Error

-- Ultimate parser - source to error or some ASTs
parseToAST :: Text -> Either Error [Expr]
parseToAST t = case parseOnly parseExprs' t of
  Left msg -> Left $ ParseError msg
  Right r -> Right r
  where parseExprs' =
          (endOfInput $> [])
          <|> do
          expr <- parseExpr
          skipSpace
          exprs <- parseExprs'
          return (expr:exprs)
  
-- -- Parse Value series

-- Parse Logic
parseLogic :: Parser Value
parseLogic = (string "True" $> Logic True)
         <|> (string "False" $> Logic False)

-- Parse Number
parseNumber :: Parser Value
parseNumber = double >>= return . Number

-- Parse Nil  
parseNil :: Parser Value
parseNil = string "nil" $> Nil

-- parse Char/String

parseEscapeChar :: Parser Char
parseEscapeChar = do
  char '\\'
  ((char 't' $> '\t')
   <|> (char '0' $> '\0')
   <|> (char 'a' $> '\a')
   <|> (char 'b' $> '\b')
   <|> (char 'f' $> '\f')
   <|> (char 'n' $> '\n')
   <|> (char 'r' $> '\r')
   <|> (char 't' $> '\t')
   <|> (char 'v' $> '\v')
   <|> (char '"' $> '\"')
   <|> (char '\'' $> '\'')
   <|> (char '\\' $> '\\'))
  
parseChar :: Parser Value
parseChar = do
  char '\''
  c <- parseEscapeChar <|> anyChar
  char '\''
  return (Character c)

-- String literal is just syntactic sugar
parseString :: Parser Value
parseString = do
  char '\"'
  parseStringToEnd
  where parseStringToEnd = do
          h <- (parseEscapeChar >>= return . Just) -- escape chars
            <|> (char '\"' >> return Nothing) -- the end of the string
            <|> (anyChar >>= return . Just)
          case h of
            Nothing -> return Nil
            Just c -> do
              str' <- parseStringToEnd
              return (Pair (Character c) str')

-- combine them all
parseValue :: Parser Value
parseValue = parseLogic
         <|> parseNumber
         <|> parseNil
         <|> parseChar
         <|> parseString

-- -- parse expressions series

-- inside parenthesis decorator
insideParent :: Parser m -> Parser m
insideParent p = do
  char '('
  skipSpace
  r <- p
  skipSpace
  char ')'
  return r

parseSExprUnitary :: Text -> (Expr -> Expr) -> Parser Expr
parseSExprUnitary fId cons = insideParent $ do
  string fId
  skipSpace
  expr <- parseExpr
  return (cons expr)

parseSExprBinary :: Text -> (Expr -> Expr -> Expr) -> Parser Expr
parseSExprBinary fId cons = insideParent $ do
  string fId
  skipSpace
  expr1 <- parseExpr
  skipSpace
  expr2 <- parseExpr
  return (cons expr1 expr2)

parseExpr :: Parser Expr
parseExpr =
     (parseValue >>= return . Lit) -- literal value
  <|> parseSExprUnitary "not" Not
  <|> parseSExprBinary "and" And
  <|> parseSExprBinary "or" Or
  <|> parseSExprBinary "+" Sum
  <|> parseSExprBinary "-" Difference
  <|> parseSExprBinary "*" Product
  <|> parseSExprBinary "/" Quotinant
  <|> parseSExprBinary "=" Equal
  <|> parseSExprBinary "<" Less
  <|> parseSExprBinary "<=" LessEqual
  <|> parseSExprBinary ">" Greater
  <|> parseSExprBinary ">=" GreaterEqual
  <|> parseSExprBinary "cons" Cons
  <|> parseSExprUnitary "car" Car
  <|> parseSExprUnitary "cdr" Cdr
  -- functional programming paradigm
  <|> parseLambda
  <|> parseApply
  <|> parseIf
  <|> parseLet
  <|> (parseAtom >>= return . Atom)

parseAtom :: Parser String
parseAtom = do
  a <- getWord
  if (null a) then fail "" else return a
  where getChar = (letter >>= (return . Just)) <|> return Nothing
        -- TODO: alphabetic char proper?
        getWord = do
          h <- getChar
          case h of
            Just c -> do
              rstr <- getWord
              return (c:rstr)
            Nothing -> return ""

parseLambda :: Parser Expr
parseLambda = insideParent $ do
  string "lambda"
  skipSpace
  a <- parseAtom
  skipSpace
  expr <- parseExpr
  return (Lambda a expr)

parseApply :: Parser Expr
parseApply = insideParent $ do
  f <- parseExpr
  skipSpace
  p <- parseExpr
  return (Apply f p)

parseIf :: Parser Expr
parseIf = insideParent $ do
  string "if"
  skipSpace
  cond <- parseExpr
  skipSpace
  tv <- parseExpr
  skipSpace
  fv <- parseExpr
  return (If cond tv fv)

-- `let` is just syntactic sugar
parseLet :: Parser Expr
parseLet = insideParent $ do
  string "let"
  skipSpace
  k <- parseAtom
  skipSpace
  v <- parseExpr
  skipSpace
  e <- parseExpr
  return (Apply (Lambda k e) v)
