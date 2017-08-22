module Parser where

import Term
import Control.Monad.Except
import Text.Parsec

type PParser = Parsec String Int

get :: PParser String
get = do x <- getState
         modifyState (+1)
         return ("x" ++ show(x))

addTipo :: Tipo -> Tipo -> Tipo
addTipo (t1 :>: t2) tt = t1 :>: (addTipo t2 tt)
addTipo t1 tt = t1 :>: tt

lit :: PParser Term
lit = do spaces
         s <- many1 (alphaNum)
         l <- get
         return ((V l) ::: (T s))

parensFormula :: PParser Term
parensFormula = do spaces
                   char '('
                   (f,_) <- formula
                   char ')'
                   return f

preOp :: PParser Term
preOp = lit <|>
        parensFormula

binOp :: PParser Term
binOp = do f <- preOp
           spaces
           binOp' f

binOp' :: Term -> PParser Term
binOp' t@(te1:::ti1) = (do string "&"
                           spaces
                           f@(te2:::ti2) <- preOp
                           spaces
                           x <- get
                           binOp' $ (t :*: f) ::: (ti1 :&: ti2)
                       )<|>
                       (do string "|"
                           spaces
                           (_:::ti2) <- preOp
                           spaces
                           x <- get
                           binOp' $ (V x) ::: (ti1 :|: ti2)
                       )<|>(binOp'' t)

binOp'' :: Term -> PParser Term
binOp'' t@(te1:::ti1) = (do string "->"
                            spaces
                            f@(_:::ti2) <- binOp
                            x <- get
                            spaces
                            binOp' $ (LamT x ti1 f) ::: (addTipo ti1 ti2)
                        )<|>(return t)

preParensFormula :: PParser Term
preParensFormula = do f <- parensFormula
                      binOp' f

formula :: PParser (Term, Int)
formula = do  r <- (binOp <|> preOp)
              formula' r

formula' :: Term -> PParser (Term,Int)
formula' a = (do r <- (binOp <|> preOp)
                 formula' r)
             <|> (
             do i <- getState
                return (a,i))

runp :: String -> Int -> Either ParseError (Term,Int)
runp s n = runParser (spaces >> formula) n "" s
