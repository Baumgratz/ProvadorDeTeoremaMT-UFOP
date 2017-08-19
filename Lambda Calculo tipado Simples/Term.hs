module Term where

type Var = String

type Contexto = [(Var, Tipo)]

data Term = V Var
          | Term :@: Term
          | Term ::: Tipo
          | Term :&: Term
          | Term :!: Term
          | Lam Var Term
          | LamT Var Tipo Term
      deriving (Show, Eq)

data Tipo = T Var
          | Tipo :>: Tipo
          | Tipo :$: Tipo
          | Tipo :|: Tipo
      deriving (Show, Eq)
