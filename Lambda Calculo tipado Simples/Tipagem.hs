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
curryTerm c (TLeft te ::: t@(a :|: b)) = do ti <- curryTerm c te
                                            case (a == ti) of
                                               True -> return t
                                               False -> throwError "Tipo errado"
curryTerm c (TRight te ::: t@(a :|: b)) = do ti <- curryTerm c te
                                             case (b == ti) of
                                                True -> return t
                                                False -> throwError "Tipo errado"
curryTerm c (te ::: ti) = do a <- curryTerm c te
                             case (a == ti) of
                               True -> return ti
                               False -> throwError "Tipo errado"
curryTerm c (t1 :*: t2) = do a <- curryTerm c t1
                             b <- curryTerm c t2
                             return (a :&: b)
curryTerm c (Fst t) = case curryTerm c t of
                            Right (p1 :&: p2) -> return p1
                            _ -> throwError "TIpo errado"
curryTerm c (Snd t) = case curryTerm c t of
                            Right (p1 :&: p2) -> return p2
                            _ -> throwError "TIpo errado"
curryTerm c (TEither (te1 ::: (a :|: b)) te2 te3) = case curryTerm c te2 of
                                                      Right t1 -> case curryTerm c te3 of
                                                                    Right t2 -> case (lastT t1 == (lastT t2)) of
                                                                                  True -> return $ lastT t1
                                                                                  False -> throwError "Error"
                                                                    Left _ -> throwError "Tipo errado"
                                                      Left _ -> throwError "tipo Errado"

lastT :: Tipo -> Tipo
lastT (t1 :>: t2) = lastT t2
lastT t1 = t1

un :: Bool -> Tipo -> String -> Result Tipo
un b t s
  | b = return t
  | otherwise = throwError s

termCont :: Contexto -> Term -> Contexto
termCont c a@((V v) ::: t) = (v,t):c
termCont c (a@(V _)) = c
termCont c (Snd a) = x
  where
    (x,y) = termCont c a
termCont c (Fst a) = x
  where
    (x,y) = termCont c a
termCont c (a ::: t) = x
  where
     (x,y) = termCont c a
termCont c (t1 :@: t2) = w
  where
     (x,y) = termCont c t1
     (w,z) = termCont x t2
termCont c (t1 :*: t2) = w
  where
     x = termCont c t1
     w = termCont x t2
termCont c (LamT v s t) = x
  where
     x = termCont c t
