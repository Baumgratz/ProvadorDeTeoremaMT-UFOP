module TermType where

data TermInf = Ann TermCheck Type
             | Bound Int
             | Free Name
             | Checking TermInf TermCheck
      deriving (Show, Eq)

data TermCheck = Inf TermInf
               | Lam TermCheck
      deriving (Show, Eq)

data Name = Global String
          | Local Int
          | Quote Int
      deriving (Show, Eq)

data Type = TFree Name
          | Fun Type Type
      deriving (Show, Eq)

data Value = VLam (Value -> Value)
           | VNeutral Neutral

data Neutral = NFree Name
             | NApp Neutral Value

vfree :: Name -> Value
vfree n = VNeutral (NFree n)
