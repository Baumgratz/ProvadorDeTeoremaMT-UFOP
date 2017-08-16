module Shell where

import System.Console.Readline
import Control.Monad.Except
import TermType
import EvalTerm
import EvalType
import Quote

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
