module Tipagem where

import Term
import Control.Monad.Except

type Result a = Either String a

curryTerm :: Contexto -> Term -> Result Tipo
curryTerm c (V v) = case lookup v c of
                      Just t -> return t
                      Nothing -> throwError $ (show v) ++ " não esta no contexto"
curryTerm c (t1 :@: t2) = case curryTerm c t1 of
                            Right (p1 :>: p2) -> do l <- curryTerm c t2
                                                    un (p1 == l) p2 "Tipo errado"
                            _ -> throwError "TIpo errado"
curryTerm c (LamT v t te) = do a <- curryTerm ((v,t):c) te
                               return (t :>: a)
curryTerm c (Lam v te) = undefined
curryTerm c (te ::: ti) = do a <- curryTerm c te
                             case (a == ti) of
                               True -> return ti
                               False -> throwError "Tipo errado"
curryTerm c (t1 :&: t2) = do a <- curryTerm c t1
                             b <- curryTerm c t2
                             return (a :$: b)
curryTerm c (t1 :!: t2) = do a <- curryTerm c t1
                             b <- curryTerm c t2
                             return (a :|: b)
un :: Bool -> Tipo -> String -> Result Tipo
un b t s
  | b = return t
  | otherwise = throwError s
