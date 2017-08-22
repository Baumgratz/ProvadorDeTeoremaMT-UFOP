module Tipagem (curryTerm,termCont)where

import Term
import Control.Monad.Except

type Result a = Either String a

type Contexto = [(String, Tipo)]

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
-- curryTerm c (te ::: t@(a :|: b)) = do ti <- curryTerm c te
--                                       case ((a == ti) || (b == ti)) of
--                                          True -> return t
--                                          False -> throwError "Tipo errado"
curryTerm c (te ::: ti) = do a <- curryTerm c te
                             case (a == ti) of
                               True -> return ti
                               False -> throwError "Tipo errado"
curryTerm c (t1 :*: t2) = do a <- curryTerm c t1
                             b <- curryTerm c t2
                             return (a :&: b)
curryTerm c (t1 :+: t2) = do a <- curryTerm c t1
                             b <- curryTerm c t2
                             return (a :|: b)
curryTerm c (Fst t) = case curryTerm c t of
                            Right (p1 :&: p2) -> return p1
                            _ -> throwError "TIpo errado"
curryTerm c (Snd t) = case curryTerm c t of
                            Right (p1 :&: p2) -> return p2
                            _ -> throwError "TIpo errado"

-- (Snd x) ::: b

un :: Bool -> Tipo -> String -> Result Tipo
un b t s
  | b = return t
  | otherwise = throwError s

termCont :: Contexto -> Term -> (Contexto, Term)
termCont c a@((V v) ::: t) = ((v,t):c,a)
termCont c (a@(V _)) = (c,a)
termCont c (Snd a) = (x,Snd y)
  where
    (x,y) = termCont c a
termCont c (Fst a) = (x,Fst y)
  where
    (x,y) = termCont c a
termCont c (a ::: t) = (x,y:::t)
  where
     (x,y) = termCont c a
termCont c (t1 :@: t2) = (w,y:@:z)
  where
     (x,y) = termCont c t1
     (w,z) = termCont x t2
termCont c (t1 :*: t2) = (w,y:*:z)
  where
     (x,y) = termCont c t1
     (w,z) = termCont x t2
termCont c (t1 :+: t2) = (w,y:+:z)
  where
     (x,y) = termCont c t1
     (w,z) = termCont x t2
termCont c (LamT v s t) = (x,LamT v s y)
  where
     (x,y) = termCont c t
