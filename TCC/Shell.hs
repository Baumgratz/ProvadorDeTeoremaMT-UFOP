module Shell where

import System.Console.Readline

data Teorema =
     Lit String
   | Imp String Teorema
      deriving (Eq)

data Type =
   Kind
   | App Type Type
   deriving (Eq)

instance Show Type where
   show Kind = "*"
   show (App t t1) = show(t) ++ " -> " ++ show(t1)

instance Show Teorema where
   show (Lit x) = x
   show (Imp x y) = x ++ " -> " ++ (show y)

type Contexto = [(Teorema, Type)]
type Prova = [(Teorema, Type)]

-- poder colocar nome
---------------------------------------------------------------------
---------------------------------------------------------------------

have' :: Eq a => [a] -> [a] -> Bool
have' []     _      = True
have' _      []     = False
have' (b:bs) (a:as)
   | a == b = have' bs as
   | otherwise = False

have :: Eq a => [a] -> [a] -> Bool
have []     _       = True
have _      []      = False
have y@(b:bs) (a:as)
   | a == b = if have' bs as then True else have y as
   | otherwise = have y as

string2Teorema :: String -> (Teorema, Type) -- ARRUMAR FUNCAO
string2Teorema [] = error "Nao existe"
string2Teorema ('\\':ls) = (Imp i l, App Kind t)
   where
      ff :: String -> String
      ff [] = []
      ff (x:xs)
         | x == '.' = []
         | otherwise = [x] ++ ff xs
      i = ff ls
      (l,t) = s2t $ removeS ls (i++".")
string2Teorema x = (Lit x, Kind)

s2t :: String -> (Teorema, Type)
s2t = string2Teorema

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

---------------------------------------------------------------------
---------------------------------------------------------------------


havePre :: String -> Bool
havePre = have "premissa"

haveGoal :: String -> Bool
haveGoal = have "goal"

removePre :: String -> String
removePre s = tail $ removeS s "premissa"

removeGoal :: String -> String
removeGoal s = tail $ removeS s "goal"

readl :: IO String
readl = do
   x <- readline ""
   return $ maybe (error "Não conseguiu ler o que digitou") id x
---------------------------------------------------------------------
---------------------------------------------------------------------

contexto' :: String -> Contexto -> IO (String,Contexto) -- return (String, Contexto)
contexto' f cont
   | havePre f = do
      s <- readl
      contexto' s $ cont ++ [s2t $ removePre f]
   | otherwise = return (f,cont)

contexto :: IO (String,Contexto)
contexto = do
   s <- readl
   contexto' s []

preContexto :: IO (String,Contexto)
preContexto = do
   cont <- contexto
   return cont

---------------------------------------------------------------------
---------------------------------------------------------------------

provar :: String -> Contexto -> IO (String, Prova)
provar f cont
   | haveGoal f = do
      s <- readl
      provar s $ cont ++ [s2t $ removeGoal f]
   | otherwise = return (f, cont)

preProvar :: String -> IO (String, Prova)
preProvar s = do
   cont <- provar s []
   return cont

---------------------------------------------------------------------
---------------------------------------------------------------------

evalProva :: Contexto -> Prova -> (Contexto, Prova)
evalProva c []             = (c,[])
evalProva c (((Imp a b), App t t1):xs) = evalProva (x ++ [(Lit a, t)]) ((b, t1):xs)
   where
      (x,y) = evalProva c xs
evalProva c (x:xs) = (i, x:j)
   where
      (i,j) = evalProva c xs

analiseProvar :: Contexto -> Prova -> IO (Contexto, Prova) -- melhorar essa funcao!!!!
analiseProvar c p  = return $ evalProva c p

analiseProvar' :: Contexto -> Prova -> IO (Contexto, Prova)
analiseProvar' c [] = return (c, [])
analiseProvar' c (((Imp a b), App t t1):xs) = do
   (x,y) <- analiseProvar' c xs
   putStrLn $ "Ultilizando a regra da introducao da implicacao o literal " ++ a ++
      " foi colocado no contexto e agora tem que provar " ++ (show b)
   analiseProvar (x ++ [(Lit a, t)]) ((b, t1):xs)
analiseProvar' c (x:xs) = do
   (i, j) <- analiseProvar' c xs
   return (i, x:j)

---------------------------------------------------------------------
---------------------------------------------------------------------

remove :: Eq a => a -> [a] -> [a]
remove x [] = []
remove x (a:as)
   | a == x = remove x as
   | otherwise = a : remove x as

foiProvado :: Contexto -> Prova -> IO Prova
foiProvado [] p = return p
foiProvado (x:xs) y
   | have [x] y = do
      putStrLn $ "O termo " ++ (show x) ++ " foi provado!"
      foiProvado xs $ remove x y
   | otherwise = foiProvado xs y

---------------------------------------------------------------------
---------------------------------------------------------------------

fim :: IO ()
fim = do
   putStrLn $ "Todas as provas ja foram provadas!!"

loop :: String -> Contexto -> Prova -> IO ()
loop s c p = do
   putStrLn $ "Contexto: " ++ (show c)
   putStrLn $ "O que Provar: " ++ (show p)

---------------------------------------------------------------------
---------------------------------------------------------------------

main = do
   putStrLn "Bem vindo ao assistente de prova!!!"
   (s,c) <- preContexto
   (x,p) <- preProvar s
   (a,b) <- analiseProvar' c p
   f <- foiProvado a b
   case f of
      [] -> fim
      otherwise -> loop x a f
   putStrLn "Fim"
