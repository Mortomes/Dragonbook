module TargetMachine where

import qualified Data.Map as M

newtype Register = Register { reg ::Int } deriving Eq

instance Show Register where show (Register i) = 'R': show i

data Location
  = Var String
  | Indexed String Register
  | IndexedInt Int Register
  | Indirect Register
  | IndexedIndirect Int Register
  | Immediate Int
  | Reg Register
  deriving Eq

instance Show Location where
  show (Var s) = s
  show (Indexed s r) = s ++ "(" ++ show r ++ ")"
  show (IndexedInt i r) = show i ++ "(" ++ show r ++ ")"
  show (Indirect r) = '*': show r
  show (IndexedIndirect i r) = "*" ++ show i ++ "(" ++ show r ++ ")"
  show (Immediate i) = '#' : show i
  show (Reg r) = show r

newtype Label = Label String deriving Eq

instance Show Label where show (Label l) = l

data Instr
  = LD Location Location
  | ST Location Register
  | ADD Location Location Location
  | SUB Location Location Location
  | MUL Location Location Location
  | DIV Location Location Location
  | BR Label
  | BLTZ Register Label
  | BGTZ Register Label
  | BEQZ Register Label
  | HALT
  deriving (Eq, Show)

class Cost x where
  cost :: x -> Int

instance Cost Location where
  cost (Var _) = 1
  cost (Indexed _ _) = 1
  cost (IndexedInt _ _) = 1
  cost (Indirect _) = 1
  cost (IndexedIndirect _ _) = 2
  cost (Immediate _) = 1
  cost (Reg _) = 0

instance Cost Instr where
  cost (LD l1 l2) = 1 + (cost l1) + (cost l2)
  cost (ST l1 _) = 1 + (cost l1)
  cost (ADD l1 l2 l3) = 1 + (cost l1) + (cost l2) + (cost l3)
  cost (SUB l1 l2 l3) = 1 + (cost l1) + (cost l2) + (cost l3)
  cost (MUL l1 l2 l3) = 1 + (cost l1) + (cost l2) + (cost l3)
  cost (DIV l1 l2 l3) = 1 + (cost l1) + (cost l2) + (cost l3)
  cost (BR _) = 1
  cost (BLTZ _ _) = 1
  cost (BGTZ _ _) = 1
  cost (BEQZ _ _) = 1
  cost HALT = 1

data MachineWord
  = Instr Instr
  | WordLit Int
  deriving (Eq, Show)

wordVal :: MachineWord -> Maybe Int
wordVal (Instr _) = Nothing
wordVal (WordLit x) = Just x

newtype Memory = Memory {mem :: [MachineWord]} deriving (Eq, Show)

data Computer = Computer
  { memory :: Memory
  , env :: M.Map String Int
  , sp :: Int
  , pc :: Int
  , registers :: [Int]}
  deriving (Eq, Show)

getMem ::  Memory -> Int -> Maybe MachineWord
getMem (Memory mem) x | x `mod` 4 /= 0 = Nothing
                      | otherwise = Just $ mem !! (x `div` 4)

replace :: Int -> a -> [a] -> [a]
replace _ _ [] = []
replace 0 y (x:xs) = y:xs
replace n y (x:xs) = x:(replace (n-1) y xs)

setMem :: Memory ->  MachineWord -> Int -> Maybe Memory
setMem (Memory mem) val loc
  | loc `mod` 4 /= 0 = Nothing
  | otherwise = Just $ Memory $ replace (loc `div` 4) val mem

getLoc :: Location -> Computer -> Maybe MachineWord
getLoc (Var x) c = M.lookup x (env c) >>= getMem (memory c)
getLoc (Indexed x r) c = do
  xVal <- getLoc (Var x) c >>= wordVal
  rVal <- getLoc (Reg r) c >>= wordVal
  getMem (memory c) (xVal + rVal)
getLoc (IndexedInt x r) c = do
  rVal <- getLoc (Reg r) c >>= wordVal
  getMem (memory c) (x + rVal)
getLoc (Indirect r) c = do
  rVal <- getLoc (Reg r) c >>= wordVal
  getMem (memory c) rVal
getLoc (IndexedIndirect x r ) c = do
  rVal <- getLoc (Reg r) c >>= wordVal
  memVal <- getMem (memory c) (x + rVal) >>= wordVal
  getMem (memory c) memVal
getLoc (Immediate x) c = Just $ WordLit x
getLoc (Reg r) c
  | (length $ registers c) > (reg r) = Just $ WordLit $ (registers c) !! (reg r)
  | otherwise = Nothing

changeMemory :: Computer -> Memory -> Computer
changeMemory (Computer m e s p r) m' = Computer m' e s p r

changeRegister :: Computer -> Int -> Int -> Maybe Computer
changeRegister (Computer m e s p r) i x
  | (length r) > i = Just $ Computer m e s p (replace x i r)
  | otherwise = Nothing
      

setLoc :: Location -> MachineWord -> Computer -> Maybe Computer
setLoc (Var x) y c = do
  xVal <- M.lookup x (env c)
  newMem <- setMem (memory c) y xVal
  Just $ changeMemory c newMem
setLoc (Indexed x r) y c = do
  rVal <- getLoc (Reg r) c >>= wordVal
  xVal <- getLoc (Var x) c >>= wordVal
  newMem <- setMem (memory c) y (rVal + xVal)
  return $ changeMemory c newMem
setLoc (IndexedInt x r) y c = do
  rVal <- getLoc (Reg r) c >>= wordVal
  newMem <- setMem (memory c) y (rVal + x)
  return $ changeMemory c newMem
setLoc (Indirect r) y c = do
  rVal <- getLoc (Reg r) c >>= wordVal
  newMem <- setMem (memory c) y rVal
  return $ changeMemory c newMem
setLoc (IndexedIndirect x r) y c = do
  rVal <- getLoc (Reg r) c >>= wordVal
  memVal <- getMem (memory c) (x + rVal) >>= wordVal
  newMem <- setMem (memory c) y memVal
  return $ changeMemory c newMem
setLoc (Immediate _) _ _ = Nothing
setLoc (Reg r) y c = do
  yVal <- wordVal y
  changeRegister c (reg r) yVal
