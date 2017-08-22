module Term where

type Var = String

-- | Term :+: Term
-- show (t1 :+: t2) = "( " ++ (show t1) ++ " | " ++ (show t2) ++ " )"
data Term = V Var
          | Term :@: Term
          | Term ::: Tipo
          | Term :*: Term
          | Lam Var Term
          | LamT Var Tipo Term
          | Fst Term
          | Snd Term
          | TLeft Term
          | TRight Term
          | TEither Term Term Term
          | Term :+: Term

instance Show Term where
  show (V v) = v
  show (t1 :@: t2@(_ :@: _)) = (show t1) ++ " ( " ++ (show t2) ++ " )"
  show (t1 :@: t2) = (show t1) ++ " " ++ (show t2)
  show (te ::: ti) = (show te) ++ " : " ++ (show ti)
  show (t1 :*: t2) = "( " ++ (show t1) ++ " , " ++ (show t2) ++ " )"
  show (LamT v ti te) = "\\(" ++ v ++ "::" ++ (show ti) ++ "). {" ++ (show te) ++ "}"
  show (Lam v te) = "\\(" ++ v ++ "). {" ++ (show te) ++ "}"
  show (Fst t) = "FST" ++ (show t)
  show (Snd t) = "SND (" ++ (show t) ++ ")"
  show (TLeft t) = "FST" ++ (show t)
  show (TRight t) = "SND (" ++ (show t) ++ ")"
  show (t1 :+: t2) = "( " ++ (show t1) ++ " | " ++ (show t2) ++ " )"

instance Eq Term where
  (V v) == (V v1) = v == v1
  (a :@: b) == (c :@: d) = a == c && b == d
  (a ::: b) == (c ::: d) = a == c && b == d
  (a :*: b) == (c :*: d) = a == c && b == d
  (a :+: b) == (c :+: d) = a == c && b == d
  (Fst t1) == (Fst t2) = t1 == t2
  (Snd t1) == (Snd t2) = t1 == t2
  (Lam v1 t1) == (Lam v2 t2) = v1 == v2 && t1 == t2
  (LamT v1 ti1 te1) == (LamT v2 ti2 te2) = v1 == v2 && ti1 == ti2 && te1 == te2



data Tipo = T Var
          | Tipo :>: Tipo
          | Tipo :&: Tipo
          | Tipo :|: Tipo
          | TFalse
        deriving (Eq)

instance Show Tipo where
  show (T v) = v
  show (t1@(_ :>: _) :>: t2) = "( " ++ (show t1) ++ " ) -> " ++ (show t2)
  show (t1 :>: t2) = (show t1) ++ " -> " ++ (show t2)
  show (t1 :&: t2) = "( " ++ (show t1) ++ " , " ++ (show t2) ++ " )"
  show (t1 :|: t2) = "( " ++ (show t1) ++ " , " ++ (show t2) ++ " )"
  show TFalse = "False"
