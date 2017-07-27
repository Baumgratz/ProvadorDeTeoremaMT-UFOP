module Shell2 where

import Tipos
import System.Console.Readline

---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
--
-- type Contexto = [(Nome, Termo, Tipo)]
-- type Prova = [(Nome, Termo, Tipo)]

---------------------------------------------------------------------------------
---------------------------------------------------------------------------------

string2Termo :: String -> Termo
string2Termo [] = error "Não Existe o termo"
string2Termo ('\\':ls) = undefined
   where
      dot' :: String -> String -> [String]
      dot' []       s = [s]
      dot' ('.':xs) s = [s] ++ dot' xs ""
      dot' (x:xs)   s = dot' xs (s++[x])
      dot :: String -> [String]
      dot s = dot' s ""
      (a:as) = dot ls
      param = spaces a

string2Termo x = Lit x
-- string2Termo x

s2t :: String -> Termo
s2t = string2Termo

ls2t :: String -> (Nome, Termo, Tipo)
ls2t = undefined

spaces' :: String -> String -> [String]
spaces' []       s = [s]
spaces' (' ':xs) s = [s] ++ spaces' xs ""
spaces' (x:xs)   s = spaces' xs (s ++ [x])

spaces :: String -> [String]
spaces s = spaces' s ""

termo2type :: Termo -> [(Nome, Termo, Tipo)] -> Tipo
termo2type (Lit _)   f = Kind
termo2type (Ap x y)  f = App (termo2type x f) (termo2type y f)
termo2type (Lam p t) f = undefined

removeS' :: Eq a => [a] -> [a] -> [a]
removeS' s      []       = s
removeS' []     _        = []
removeS' x@(a:as) (b:bs)
   | a == b = removeS' as bs
   | otherwise = x

removeS :: Eq a => [a] -> [a] -> [a]
removeS s      []       = s
removeS (a:as) x@(b:bs)
   | a == b = removeS' as bs
   | otherwise = a : removeS as x

removePre :: String -> String
removePre s = removeS s "premissa "

removeGoal :: String -> String
removeGoal s = removeS s "goal "

removeLet :: String -> String
removeLet s = removeS s "let "

readl :: String -> IO String
readl s = do
   x <- readline s
   return $ maybe (error "Não conseguiu ler o que digitou") id x
---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
loop' :: String -> (Contexto, Prova) -> IO (Contexto, Prova) -- return (String, Contexto)
loop' f@(x:xs) a@(cont, pro) = undefined
   -- | x == 'p' = do
   --    s <- readl ""
   --    loop' s (cont ++ [s2t $ removePre f], pro)
   -- | x == 'g' = do
   --    s <- readl ""
   --    loop' s (cont, pro ++ [s2t $ removeGoal f])
   -- | x == 'l' = do
   --    s <- readl ""
   --    loop' s   (cont ++ [ls2t $ removeLet f], pro)
   -- | otherwise = return a

loop :: IO (Contexto, Prova)
loop = do
   s <- readl ""
   loop' s ([],[])

---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
-- premissa ******
-- goal *****
-- let **** = *****
main :: IO ()
main = do
   putStrLn "Bem vindo ao assistente de prova!!!"
