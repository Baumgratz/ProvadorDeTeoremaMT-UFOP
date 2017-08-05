module TermType where

data TermInf = Ann TermCheck TermCheck -- termo :: tipo
             | Star -- Sem tipo defido
             | Pi TermCheck TermCheck
             | Bound Int -- qual o lambda referente
             | Free Name -- nome da variavel
             | TermInf :@: TermCheck -- aplicacao
      deriving (Eq)

instance Show TermInf where
   show (Ann a b)  = (show a) ++ " :: " ++ (show b)
   show Star       = "*"
   show (Pi t1 t2) = (show t1) ++ " => " ++ (show t2) -- tipo a -> a?
   show (Bound i)  = show i
   show (Free n)   = show n
   show (a :@: b)  = (show a) ++ " <> " ++ (show b)

data TermCheck = Inf TermInf -- Inferindo
               | Lam TermCheck -- lambda
      deriving (Eq)

instance Show TermCheck where
   show (Inf t) = (show t)
   show (Lam a) = "\\. (" ++ (show a) ++ ")"

data Name = Global String -- nome global
          | Local Int -- nome Local
          | Quote Int -- numero?
      deriving (Eq)

instance Show Name where
   show (Global x) = x
   show (Local i)  = show i
   show (Quote i)  = "q"++(show i)

data Value = VLam (Value -> Value) -- value = aplicacao dos termos?
           | VStar
           | VPi Value (Value -> Value)
           | VNeutral Neutral

data Neutral = NFree Name
             | NApp Neutral Value

vfree :: Name -> Value
vfree n = VNeutral (NFree n)

type Type = Value
type Context = [(Name, Type)]
