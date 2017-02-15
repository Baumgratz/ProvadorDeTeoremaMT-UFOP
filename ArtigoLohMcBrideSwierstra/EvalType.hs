module EvalType where

import TermType

data Kind = Star
      deriving (Show)

data Info = HasKind Kind
          | HasType Type
      deriving (Show)

type Context = [(Name, Info)]

type Result a = Either String a

kindCheck :: Context -> Type -> Kind -> Result ()
kindCheck cont (TFree x) Star =
   case lookup x cont of
      Just (HasKind Star) -> return ()
      Nothing             -> error "nao identificado"
kindCheck cont (Fun k k') Star =
   do kindCheck cont k Star
      kindCheck cont k' Star

typeInf0 :: Context -> TermInf -> Result Type
typeInf0 = typeInf 0

typeInf :: Int -> Context -> TermInf -> Result Type
typeInf i cont (Ann e t) =
   do kindCheck cont t Star
      typeCheck i cont e t
      return t
typeInf i cont (Free x) =
   case lookup x cont of
      Just (HasType t) -> return t
      Nothing          -> error "nao identificado"
typeInf i cont (Checking e e') =
   do a <- typeInf i cont e
      case a of
         Fun t t' -> do typeCheck i cont e' t
                        return t'
         _        -> error "aplicação errada"

typeCheck :: Int -> Context -> TermCheck -> Type -> Result ()
typeCheck i cont (Inf e) t =
   do t' <- typeInf i cont e
      unless t t' ("tipo errado")
typeCheck i cont (Lam e) (Fun t t') = typeCheck (i+1) ((Local i, HasType t):cont) (substCheck 0 (Free (Local i)) e) t'
typeCheck i cont _ _ = error "tipo errado"

substInf :: Int -> TermInf -> TermInf -> TermInf
substInf i r (Ann e t)   = Ann (substCheck i r e) t
substInf i r b@(Bound j)
   | i == j = r
   | otherwise = b
substInf i r f@(Free y) = f
substInf i r (Checking e e') = Checking (substInf i r e) (substCheck i r e')

substCheck :: Int -> TermInf -> TermCheck -> TermCheck
substCheck i r (Inf e) = Inf (substInf i r e)
substCheck i r (Lam e) = Lam $ substCheck (i+1) r e

unless :: Eq a => a -> a -> String -> Result ()
unless t t' s
   | t == t' = return ()
   | otherwise = error s
