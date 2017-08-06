module Ler where

import System.Console.Readline
import Control.Monad.Except
import TermType
import TermType
import EvalTerm
import EvalType
import Quote

cut' :: Eq a => [a] -> a -> [a] -> ([a], [a])
cut' [] _ s = (s,[])
cut' (x:xs) c s
   | x == c = (s,xs)
   | otherwise = cut' xs c (s ++ [x])

cut :: Eq a => [a] -> a -> ([a], [a])
cut s s' = cut' s s' []

pos' :: Eq a => Int -> a -> [a] -> Result Int
pos' n _ [] = throwError "Nao e uma variavel do lambda"
pos' n a (x:xs)
   | a == x = Right n
   | otherwise = pos' (n+1) a xs

pos :: Eq a =>  a -> [a] -> Result Int
pos = pos' 0

ipi :: TermCheck -> TermCheck -> TermCheck
ipi a b = Inf $ Pi a b

tfree :: String -> Value
tfree a = VNeutral $ NFree $ Global a

varfree :: String -> TermInf
varfree s = Free $ Global s

s2term :: String -> String -> [TermCheck]
s2term [] s = [Inf (Free (Global s))]
s2term q@(x:xs) s = undefined

----------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------

string2term :: String -> Context -> String -> TermCheck
string2term [] c s = Inf (varfree s)
string2term (x:y:xs) c s
   | x == '\\' = undefined -- lam2term xs c ""
   | x == '(' && y /= ' '  = Inf $ varfree s :@: p1 :@: b1
   | x == '(' && y == ' '  = b2
   | x == ' '  = Inf $ varfree s :@: t
   | otherwise = string2term (y:xs) c (s++[x])
      where
         t = string2term (y:xs) c ""
         (a1, p1) = par2string xs c ""
         b1 = string2term a1 c ""
         (a2, p2) = par2string (y:xs) c ""
         b2 = string2term a2 c ""

----------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------

par2string :: String -> Context -> String -> (String, TermCheck)
par2string [] c s = error "Parser errado"
par2string (x:y:xs) c s
   | x == '(' = Inf $ varfree s :@: t :@: b
   | x == ')' && y /= ' ' = (y:xs, Inf (varfree s))
   | x == ')' && y == ' ' = (xs, Inf (varfree s))
   | x == ' ' = (a, Inf $ varfree s :@: t)
   | otherwise = par2string (y:xs) c (s++[x])
      where
         (a, t) = par2string (y:xs) c ""
         b = string2term a c ""
