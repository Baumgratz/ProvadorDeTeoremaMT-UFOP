module EvalType where

import Control.Monad.Except
import Quote
import TermType
import EvalTerm

type Result a = Either String a

typeInf0 :: Context -> TermInf -> Result Type
typeInf0 = typeInf 0

typeInf :: Int -> Context -> TermInf -> Result Type
typeInf i cont (Ann e p) =
   do typeCheck i cont p VStar
      let t = evalCheck p []
      typeCheck i cont e t
      return t
typeInf i cont Star = return VStar
typeInf i cont (Pi p p') =
   do typeCheck i cont p VStar
      let t = evalCheck p []
      typeCheck (i+1) ((Local i, t): cont) (substCheck 0 (Free (Local i)) p') VStar
      return VStar
typeInf i cont (Free x) =
   case lookup x cont of
      Just t  -> return t
      Nothing -> throwError $ (show x) ++ " nao esta no contexto\n"
typeInf i cont (e :@: e') =
   do a <- typeInf i cont e
      case a of
         VPi t t' -> do typeCheck i cont e' t
                        return (t' (evalCheck e' []))
         _        -> throwError "aplicação incorreta"

typeCheck :: Int -> Context -> TermCheck -> Type -> Result ()
typeCheck i cont (Inf e) v =
   do v' <- typeInf i cont e
      unless (quote0 v == quote0 v') $ throwError "tipo incorreto"
typeCheck i cont (Lam e) (VPi t t') = typeCheck (i+1) ((Local i, t):cont) (substCheck 0 (Free (Local i)) e) $ t' $ vfree $ Local i
typeCheck i cont _ _ = throwError "tipo incorreto"

substInf :: Int -> TermInf -> TermInf -> TermInf
substInf i r (Ann e t)   = Ann (substCheck i r e) (substCheck i r t)
substInf i r b@(Bound j)
   | i == j = r
   | otherwise = b
substInf i r f@(Free y) = f
substInf i r (e :@: e') = (substInf i r e) :@: (substCheck i r e')
substInf i r Star       = Star
substInf i r (Pi t t')  = Pi (substCheck i r t) (substCheck (i+1) r t')

substCheck :: Int -> TermInf -> TermCheck -> TermCheck
substCheck i r (Inf e) = Inf (substInf i r e)
substCheck i r (Lam e) = Lam $ substCheck (i+1) r e
