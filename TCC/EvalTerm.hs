module EvalTerm (Env, evalInf, evalCheck) where

import TermType

type Env = [Value]

evalInf :: TermInf -> Env -> Value
evalInf (Ann e _)  d = evalCheck e d
evalInf (Star)     d = VStar
evalInf (Pi t t')  d = VPi (evalCheck t d) (\x -> evalCheck t' (x:d))
evalInf (Free x)   d = vfree x
evalInf (Bound i)  d =  d !! i
evalInf (e :@: e') d = vapp (evalInf e d) (evalCheck e' d)

vapp :: Value -> Value -> Value
vapp (VLam f)     v = f v
vapp (VNeutral n) v = VNeutral (NApp n v)

evalCheck :: TermCheck -> Env -> Value
evalCheck (Inf i) d = evalInf i d
evalCheck (Lam e) d = VLam (\x -> evalCheck e (x:d))
