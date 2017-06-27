module Shell where

data Teorema =
     Lit String
   | Imp String Teorema
      deriving (Show, Eq)

type Contexto = [Teorema]
type Prova = [Teorema]

---------------------------------------------------------------------
---------------------------------------------------------------------

have' :: String -> String -> Bool
have' _ [] = True
have' [] _ = False
have' (a:as) (b:bs)
   | a == b = have' as bs
   | otherwise = False

have :: String -> String -> Bool
have _      []     = True
have []     _      = False
have (a:as) y@(b:bs)
   | a == b = if have' as bs then True else have as y
   | otherwise = have as y

string2Teorema :: String -> Teorema -- ARRUMAR FUNCAO
string2Teorema [] = error "Nao existe"
string2Teorema ('\\':ls) = Imp i l
   where
      ff :: String -> String
      ff [] = []
      ff (x:xs)
         | x == '.' = []
         | otherwise = [x] ++ ff xs
      i = ff ls
      l = s2t $ removeS i ls
string2Teorema x = Lit x

s2t :: String -> Teorema
s2t = string2Teorema

removeS' :: String -> String -> String
removeS' s      []       = s
removeS' []     _        = []
removeS' x@(a:as) (b:bs)
   | a == b = removeS' as bs
   | otherwise = x

removeS :: String -> String -> String
removeS s      []       = s
removeS (a:as) x@(b:bs)
   | a == b = removeS' as bs
   | otherwise = a : removeS as x

---------------------------------------------------------------------
---------------------------------------------------------------------


havePre :: String -> Bool
havePre s = have s "premissa"

haveGoal :: String -> Bool
haveGoal g = have g "goal"

removePre :: String -> String
removePre s = tail $ removeS s "premissa"

removeGoal :: String -> String
removeGoal s = tail $ removeS s "goal"

---------------------------------------------------------------------
---------------------------------------------------------------------

contexto' :: String -> Contexto -> IO (String,Contexto) -- return (String, Contexto)
contexto' f cont
   | havePre f = do
      s <- getLine
      contexto' s $ cont ++ [s2t $ removePre f]
   | otherwise = return (f,cont)

contexto :: IO (String,Contexto)
contexto = do
   s <- getLine
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
      s <- getLine
      provar s $ cont ++ [s2t $ removeGoal f]
   | otherwise = return (f, cont)

-- provar :: String -> IO Prova
-- provar s = do
--    s <- getLine
--    provar' s []

preProvar :: String -> IO (String, Prova)
preProvar s = do
   cont <- provar s []
   return cont

---------------------------------------------------------------------
---------------------------------------------------------------------

main = do
   putStrLn "Hello!!!"
   (s,c) <- preContexto
   (x,p) <- preProvar s
   -- s <- getLine
   putStrLn (show c)
   putStrLn (show p)
   putStrLn "Fim"
