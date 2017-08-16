module Shell where

import System.Console.Readline
import Control.Monad.Except
import TermType
import Parser

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

loop' :: String -> ([TermCheck],[TermCheck]) -> IO ([TermCheck],[TermCheck])
loop' s@(x:xs) b@(g,p)
   | x == 'p' = do
      a <- return $ retira "premissa " s
      r <- return $ either (\_-> error "Parser errado") id (run a)
      x <- readl ""
      loop' x (g,r:p)
   | x == 'g' = do
      a <- return $ retira "goal " s
      r <- return $ either (\_-> error "Parser errado") id (run a)
      x <- readl ""
      loop' x (r:g,p)
   | otherwise = return b

loop :: IO ([TermCheck],[TermCheck])
loop = do
   s <- readl ""
   loop' s ([],[])

main :: IO ()
main = do
   x <- loop
   putStrLn (show x)
   return ()
