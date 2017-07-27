module Tipos where

type Nome = String

data Termo =
   Lit String
   | Lam [String] Termo
   | Ap Termo Termo
   deriving (Eq)

instance Show Termo where
   show (Lit a) = a
   show (Lam s t2) = "\\" ++ st ++ ". " ++ show(t2)
      where
         st = foldr (\ x y -> " " ++ x ++ y) " " s
   show (Ap t1 x@(Ap _ _)) = show(t1) ++ " (" ++ show(x) ++ ")"
   show (Ap t1 t2) = show(t1) ++ " " ++ show(t2)

data Tipo =
   Kind
   | Tp String
   | App Tipo Tipo
   deriving (Eq)

instance Show Tipo where
   show Kind = "*"
   show (App x@(App _ _) t1) = "(" ++ show(x) ++ ")" ++ " -> " ++ show(t1)
   show (App t t1) = show(t) ++ " -> " ++ show(t1)
   show (Tp s) = s

type Contexto = [(Nome, Termo, Tipo)]
type Prova = [(Nome, Termo, Tipo)]
