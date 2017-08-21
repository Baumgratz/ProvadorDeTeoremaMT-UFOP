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

caseStr :: String -> String -> String -> Term -> Term -> Tipo -> Tipo -> Term
caseStr "&" _ _ te1 te2 ti1 ti2 = ((te1:::ti1) :*: (te2:::ti2)) ::: (ti1 :&: ti2)
caseStr "|" _ _ te1 te2 ti1 ti2 = ((te1:::ti1) :+: (te2:::ti2)) ::: (ti1 :|: ti2)
caseStr "->" v _ te1 te2 ti1 ti2 = LamT v ti1 (te2 ::: ti2) ::: (addTipo ti1 ti2)
caseStr "<->" v1 v2 te1 te2 ti1 ti2 = (caseStr "->" v1 "" te1 te2 ti1 ti2) :*: (caseStr "->" v2 "" te2 te1 ti2 ti1) ::: ((addTipo ti1 ti2) :&: (addTipo ti2 ti1))
-- caseStr s _ te1 _ _ _ = error $ show te1

caseTerm :: String -> String -> String -> Term -> Term -> Term
caseTerm s v1 v2 (te1 ::: ti1) (te2 ::: ti2) = caseStr s v1 v2 te1 te2 ti1 ti2

preOp :: PParser Term
preOp = lit <|>
        parensFormula

binOp :: PParser Term
binOp = do f <- preOp
           spaces
           binOp' f

binOp' :: Term -> PParser Term
binOp' t = (do string "&"
               spaces
               f <- preOp
               spaces
               binOp' $ caseTerm "&" "" "" t f
           )<|>
           (do string "|"
               spaces
               f <- preOp
               spaces

               binOp' $ caseTerm "|" "" "" t f
           )<|>(binOp'' t)

binOp'' :: Term -> PParser Term
binOp'' t = (do string "->"
                spaces
                f <- binOp
                x <- get
                spaces
                binOp' $ caseTerm "->" x "" t f
            )<|>
            (do string "<->"
                spaces
                f <- binOp
                spaces
                x <- get
                y <- get
                binOp' $ caseTerm "<->" x y t f
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
