module TypeChecking where

import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (isJust, fromJust)

type Ident = String
type TVar = String

data Type
  = Prim TVar
  | Var TVar
  | Prod [Type]
  | Constr TVar [Type]
  | Arrow Type Type
  deriving Eq

varType :: Type -> Bool
varType (Var _) = True
varType _ = False

data TypeNode = TypeNode
  { children :: [TypeNode], set :: Maybe TypeNode, type' :: Type } deriving (Eq, Show)

opNode :: TypeNode -> Bool
opNode n = (length $ children n) > 0

instance Show Type where
  show (Prim x) = x
  show (Var x) = "(" ++ x ++ ")"
  show (Prod xs) = "(" ++ (intercalate " X " (map show xs)) ++ ")"
  show (Constr n xs) = n ++ "(" ++ (intercalate " " (map show xs)) ++ ")"
  show (Arrow t1 t2) = "(" ++ (show t1) ++ " -> " ++ (show t2) ++ ")"

type Env = M.Map Ident Type

findRepresentative :: TypeNode -> TypeNode
findRepresentative node = case (set node) of
  Nothing -> node
  (Just next) -> findRepresentative next

setRepresentative :: TypeNode -> Maybe TypeNode -> TypeNode
setRepresentative n1 n2 = TypeNode (children n1) n2 (type' n1)

union :: (TypeNode, TypeNode) -> (TypeNode, TypeNode)
union (n1, n2) = let r1 = findRepresentative n1
                     r2 = findRepresentative n2
                     t1 = type' r1
                     t2 = type' r2
  in if varType t1 then ((setRepresentative r1 (Just r2)), r2)
                   else (r1, (setRepresentative r2 (Just r1)))

unify :: TypeNode -> TypeNode -> Maybe TypeNode
unify m n = let s = findRepresentative m
                t = findRepresentative n
  in if s == t then Just s else
     if (type' s) == (type' t) then Just s else
     if opNode s && (length $ children s) == (length $ children t) then
       let (us, ut) = union (s, t)
           unifiedChildren = zipWith unify (children us) (children ut)
       in if all isJust unifiedChildren then
         Just (TypeNode (map fromJust unifiedChildren) (Just us) (type' us))  else Nothing else
     if varType (type' s) || varType (type' t) then
       let (us, ut) = union (s, t)
       in Just us
     else Nothing

mkTypeNode :: Type -> TypeNode
mkTypeNode (Prim x) = TypeNode [] Nothing (Prim x)
mkTypeNode (Var x) = TypeNode [] Nothing (Var x)
mkTypeNode (Prod xs) = TypeNode (map mkTypeNode xs) Nothing (Prod xs)
mkTypeNode (Constr n xs) = TypeNode (map mkTypeNode xs) Nothing (Constr n xs)
mkTypeNode (Arrow t1 t2) = TypeNode [mkTypeNode t1, mkTypeNode t2] Nothing (Arrow t1 t2)

t1 :: Type
t1 = Prim "Integer"
n1 :: TypeNode
n1 = TypeNode [] Nothing t1

t2 :: Type
t2 = Var "a"
n2 :: TypeNode
n2 = TypeNode [] Nothing t2

t3 :: Type
t3 = Prim "Bool"
n3 :: TypeNode
n3 = TypeNode [] Nothing t3

ext1 :: Type
ext1 = Arrow
  (Prod
    [ Arrow (Var "a1") (Var "a2")
    , Constr "List" [Var "a3"]])
  (Constr "List" [Var "a2"])

exn1 :: TypeNode
exn1 = mkTypeNode ext1

ext2 :: Type
ext2 = Arrow
  (Prod
    [ Arrow (Var "a3") (Var "a4")
    , Constr "List" [Var "a3"]])
  (Var "a5")

exn2 :: TypeNode
exn2 = mkTypeNode ext2
