module Shell where

import System.Console.Readline
import Control.Monad.Except
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

s2term :: String -> String -> [TermCheck]
s2term [] s = [Inf (Free (Global s))]
s2term q@(x:xs) s
   | x == '\\' = [string2term q]
   | x == '('  = undefined
   | x == ' '  = Inf (Free (Global s)):(s2term xs "")
   | otherwise = s2term xs (x:s)
----------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------

readl :: String -> IO String
readl s = do
   x <- readline s
   return $ maybe (error "Não conseguiu ler o que digitou") id x

retira' :: String -> String -> String
retira' []     s      = s
retira' (x:xs) (y:ys)
   | x == y = retira' xs ys
   -- | otherwise = error "ERROR!!!"

retira :: String -> String -> String
retira []     s      = s
retira (x:xs) (y:ys)
   | x == y = retira' xs ys
   | otherwise = y : retira (x:xs) ys

----------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------

string2term :: String -> TermCheck
string2term [] = error "Não foi digitado nada"
string2term (x:xs)
   | x == '\\' = lam2term xs [] ""
   | x == '('  = undefined --par2string 0 xs
   | otherwise = undefined --var2term x xs

----------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------

var2term :: [TermCheck] -> TermCheck
var2term [] = undefined
var2term [z]  = z


----------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------

var2bound' :: TermInf -> [String] -> TermInf
var2bound' a@(Bound i)       _   = a
var2bound' (t1 :@: t2)       var = ( a1 :@: a2 )
   where
      a1 = var2bound' t1 var
      a2 = var2bound t2 var
var2bound' x@(Free (Global s)) var = ff p x
   where
      p = pos s var
      ff :: Result Int -> TermInf -> TermInf
      ff (Right a) _ = Bound a
      ff (Left _)  t = t

var2bound :: TermCheck -> [String] -> TermCheck
var2bound (Lam l) s = Lam t
   where
      t =  var2bound l s
var2bound (Inf i) s =  Inf t
   where
      t = var2bound' i s

lam2term' :: String -> [String] -> TermCheck
lam2term' s var = var2bound t var
   where
      t = string2term s

lam2term :: String -> [String] -> String -> TermCheck
lam2term []       _   _ = undefined --
lam2term (x:y:xs) var s
   | x == '.' && y == '\\' = lam2term xs (s:var) ""
   | x == '.' && y /= '\\' = lam2term' xs (s:var)
   | otherwise = lam2term (y:xs) var (s++[x])

----------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------

loop' :: String -> IO (String,Context)
loop' s@(x:xs)
   | x == 'p' = do
      a <- return $ retira "premissa " s
      return (a,[])
   -- | otherwise = return s



loop :: IO (String,Context)
loop = do
   s <- readl ""
   loop' s

main :: IO ()
main = do
   x <- loop
   return ()
