module CodeGeneration where

data Register = Register Int deriving Eq

instance Show Register where
  show (Register i) = 'R':(show i)

data Location
  = Address Int
  | Reg Register
  | Var String
  | IndexAddrVar String Register
  | IndexAddrConst Int Register
  | Indirect Register
  | IndexedIndirect Int Register
  | Immediate Int
  deriving Eq

instance Show Location where
  show (Address x) = show x
  show (Reg r) = show r
  show (Var s) = s
  show (IndexAddrVar v r) = v ++ "(" ++ (show r) ++ ")"
  show (IndexAddrConst x r) = (show x) ++ "(" ++ (show r) ++ ")"
  show (Indirect r) = '*':(show r)
  show (IndexedIndirect x r) = "*" ++ show x ++ "(" ++ show r ++ ")"
  show (Immediate x) = '#':(show x)

data Instr
  = LD Location Location
  | ST Location Register
  | ADD Location Location Location
  | SUB Location Location Location
  | BR Location
  | BLTZ Register Location
  | BGTZ Register Location
  | BEQZ Register Location
  deriving (Eq, Show)
