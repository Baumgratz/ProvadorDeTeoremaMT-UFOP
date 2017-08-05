module Quote (quote0)
where

import TermType

quote0 :: Value -> TermCheck
quote0 = quote 0

quote :: Int -> Value -> TermCheck
quote i (VLam f)     = Lam (quote (i+1) (f (vfree (Quote i))))
quote i VStar        = Inf Star
quote i (VPi v f)    = Inf $ Pi (quote i v) $ quote (i+1) $ vfree $ Quote i
quote i (VNeutral n) = Inf $ neutralQuote i n

neutralQuote :: Int -> Neutral -> TermInf
neutralQuote i (NFree x)  = boundfree i x
neutralQuote i (NApp n v) = (neutralQuote i n) :@: (quote i v)

boundfree :: Int -> Name -> TermInf
boundfree i (Quote k) = Bound (i-k-1)
boundfree i x         = Free x
