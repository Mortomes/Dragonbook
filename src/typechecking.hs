module TypeChecking where

import Data.List (intercalate)
import qualified Data.Map as M

type Ident = String
type TVar = String

data Type
  = Prim TVar
  | Var TVar
  | Prod [Type]
  | Constr TVar [Type]
  | Arrow Type Type
  deriving Eq

instance Show Type where
  show (Prim x) = x
  show (Var x) = "(" ++ x ++ ")"
  show (Prod xs) = "(" ++ (intercalate " X " (map show xs)) ++ ")"
  show (Constr n xs) = n ++ "(" ++ (intercalate " " (map show xs)) ++ ")"
  show (Arrow t1 t2) = "(" ++ (show t1) ++ " -> " ++ (show t2) ++ ")"
  

data Expr
  = Ident Ident
  | FunDef Ident Ident Expr
  | FunApp Ident Ident
  deriving (Eq, Show)

type Env = M.Map Ident Type
