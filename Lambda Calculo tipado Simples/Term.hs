module Term where

type Var = String

data Term = V Var
          | Term :@: Term
          | Term ::: Tipo
          | Term :*: Term
          | Term :+: Term
          | Lam Var Term
          | LamT Var Tipo Term
          | Fst Term
          | Snd Term
          | TEither Term Term
      deriving (Show, Eq)

data Tipo = T Var
          | Tipo :>: Tipo
          | Tipo :&: Tipo
          | Tipo :|: Tipo
      deriving (Show, Eq)
