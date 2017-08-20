module Parser where

import Term
import Data.Char
import Text.ParserCombinators.ReadP
import Control.Monad.Except

addTipo :: Tipo -> Tipo -> Tipo
addTipo (t1 :>: t2) tt = t1 :>: (addTipo t2 tt)
addTipo t1 tt = t1 :>: tt

variable :: ReadP Term
variable = do skipSpaces
              s <- many1 (satisfy (\x -> isLower x && isAlpha x))
              return (V "l" ::: (T s))

top :: ReadP Term
top = do skipSpaces
         char 'T'
         return (LamT "a" (TFalse) ((V "b") ::: (TFalse)) ::: (TFalse :>: TFalse))

bottom :: ReadP Term
bottom = do skipSpaces
            char 'F'
            return ((V "f") ::: TFalse)

lit :: ReadP Term
lit = top +++ bottom +++ variable

parensFormula :: ReadP Term
parensFormula = do skipSpaces
                   char '('
                   f <- formula
                   char ')'
                   return f

notOp :: ReadP Term
notOp = do skipSpaces
           char '-'
           f <- formula
           case f of
             (t1 ::: t2) -> return (t1 ::: (addTipo t2 TFalse))
             _ -> error "Error"
          --  return (f :>: TFalse) -- ???

chrOp :: ReadP String
chrOp = do (string "*"    +++
            string "+"    +++
            string "->"   +++
            string "<->")

halfOp :: Term -> ReadP Term
halfOp (te1 ::: ti1) = do skipSpaces
                          c  <- chrOp
                          f' <- formula
                          case f' of
                            (te2 ::: ti2) -> return $ caseStr c te1 te2 ti1 ti2
                            _ -> error "Error Parser"

caseStr :: String -> Term -> Term -> Tipo -> Tipo -> Term
caseStr "*" te1 te2 ti1 ti2 = (te1 :*: te2) ::: (ti1 :&: ti2)
caseStr "+" te1 te2 ti1 ti2 = (te1 :+: te2) ::: (ti1 :|: ti2)
caseStr "->" (V v) te2 ti1 ti2 =  LamT v ti1 (te2 ::: ti2) ::: (ti1 :>: ti2)
caseStr "<->" te1 te2 ti1 ti2 = (caseStr "->" te1 te2 ti1 ti2) :*: (caseStr "->" te2 te1 ti2 ti1) ::: (ti1 :&: ti2)

preOp :: ReadP Term
preOp = lit           +++
        notOp         +++
        parensFormula


binOp :: ReadP Term
binOp = do f <- preOp
           halfOp f

preParensFormula :: ReadP Term
preParensFormula = do f <- parensFormula
                      halfOp f

formula :: ReadP Term
formula = do  r <- (preOp +++ binOp)
              skipSpaces
              return r

-- rule :: ReadP Rule
-- rule = do f <- formula
--           string "|->"
--           r <- formula
--           return (Rule (f,r))

chooseResult :: [(a,String)] -> Maybe a
chooseResult [] = Nothing
chooseResult ((x,""):_) = Just x
chooseResult (_:ys) = chooseResult ys

parseFormula :: String -> Maybe Term
parseFormula s = chooseResult (readP_to_S formula s)

-- parseRule :: String -> Maybe Rule
-- parseRule s = chooseResult (readP_to_S rule s)

-- parseRules :: String -> [Rule]
-- parseRules s = [x |(Just x) <- map parseRule (lines s)]
