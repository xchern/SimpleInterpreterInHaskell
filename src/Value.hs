module Value where

import qualified Data.Map as Map -- used to store environment

type Atom = String

type Env = Map.Map Atom Value

nullEnvP :: Env -> Bool
nullEnvP = Map.null

emptyEnv :: Env
emptyEnv = Map.empty

insertBind :: Atom -> Value -> Env -> Env
insertBind a v e = Map.insert a v e

lookupEnv :: Atom -> Env -> Maybe Value
lookupEnv a e = Map.lookup a e

-- left-biased
unionEnv :: Env -> Env -> Env
unionEnv = Map.union

data Function = Function { paramF :: Atom
                         , closureF :: Closure
                         }

data Value = Logic Bool
           | Number Double
           | Nil
           | Pair Value Value
           | Character Char
           | FunctionV Function

instance Show Value where
  show (Logic b) = if b then "#t" else "#f"
  show (Number n) = show n
  show Nil = "nil"
  show (Pair a b) = "(" ++ (show a) ++ "." ++ (show b) ++ ")"
  show (Character c) = '\'' : c : "'"
  show (FunctionV _) = "<#function>"


-- Clousure - the key of implementation of lexical scope
-- considering all data are mutable,
-- upvalues can be simplified, just combine them into Env.
data Closure = Closure { envC :: Env
                       , exprC :: Expr
                       } deriving Show

-- normal expression
data Expr = Lit Value
         -- logical operation
         | Not Expr
         | And Expr Expr
         | Or Expr Expr
         -- float operation
         | Sum Expr Expr
         | Difference Expr Expr
         | Product Expr Expr
         | Quotinant Expr Expr
         | Equal Expr Expr
         | Less Expr Expr
         | LessEqual Expr Expr
         | Greater Expr Expr
         | GreaterEqual Expr Expr
         -- String/List
         | Cons Expr Expr
         | Car Expr
         | Cdr Expr
-- Functional EXPRession
-- let is just a syntactic sugar
-- (let (x v) e) => ((lambda (x) e) v)
-- so not defined here.
         | Atom Atom
         | Lambda Atom Expr
         | Apply Expr Expr
-- Conditional Expression (condition, true value, false value)
         | If Expr Expr Expr
          deriving Show
