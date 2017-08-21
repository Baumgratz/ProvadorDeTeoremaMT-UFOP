module Shell where

import Parser
import Term
import Tipagem
import Control.Monad.Except

type Premissa = [Term]
type Objetivo = Tipo
type Provas = [Term]
type Result a = Either String a

type State = (Int, Premissa, Objetivo, Provas)

elemObj :: String -> Bool
elemObj [] = False
elemObj (x:xs)
  | x == 'O' = elemObj' "bjetivo:" xs
  | otherwise = elemObj xs

elemObj' :: String -> String -> Bool
elemObj' [] _ = True
elemObj' (x:xs) (y:ys)
  | x == y = elemObj' xs ys
  | otherwise = False

noObj' :: String -> String -> String
noObj' [] s = s
noObj' (x:xs) (y:ys)
  | x == y = noObj' xs ys
  | otherwise = error "Error!!!!"


noObj :: String -> String
noObj [] = ""
noObj (x:xs)
  | x == 'O' = noObj' "bjetivo:" xs
  | otherwise = noObj xs

premissa :: IO ()
premissa = do putStr "1:"
              x <- getLine
              case elemObj x of
                True -> do y <- return $ noObj x
                           process (0,[], tObj (either (\_ -> error "Parser deu Errado") id (runp y)),[])
                False -> premissa' 2 [either (\_ -> error "Parser deu Errado") id (runp x)]

premissa' :: Int -> [Term] -> IO ()
premissa' i p = do putStr $(show i) ++ ":"
                   x <- getLine
                   case (elemObj x) of
                      True -> do y <- return $ noObj x
                                 process (i,p, tObj (either (\_ -> error "Parser deu Errado") id (runp y)),[])
                      False -> premissa' (i+1) $ (either (\_ -> error "Parser deu Errado") id (runp x)):p

tObj :: Term -> Tipo
tObj (t1 ::: t2) = t2

regras :: String -> (Int, String)
regras (x:y:z:xs)
  | 'c' == x = (0, xs)
  | 'E' == x = regrasE (y:z:xs)
  | 'I' == x = regrasI (y:z:xs)
  | otherwise = (-1, x:xs)

regrasE :: String -> (Int, String)
regrasE w@(x:y:z:xs)
  | x == '&' && y == 'e'= (1, z:xs)
  | x == '&' && y == 'd'= (2, z:xs)
  | x == '|' = (3, y:z:xs)
  | x == '-' = (4, z:xs)
  | otherwise = (-1, w)

regrasI :: String -> (Int, String)
regrasI w@(x:y:z:xs)
  | x == '&' = (5, y:z:xs)
  | x == '|' = (6, y:z:xs)
  | x == '-' = (7, z:xs)
  | otherwise = (-1, w)

fim' :: Objetivo -> Provas -> Bool
fim' a [] = False
fim' a ((t1 ::: t2):xs) = (a == t2) || (fim' a xs)

fim :: Objetivo -> Provas -> State -> IO ()
fim o p s
  | fim' o p = putStrLn "Foi provado!"
  | otherwise = process s

itens :: String -> [Int]
itens [] = []
itens [a]
  | a == ' ' = []
  | otherwise = [read [a] :: Int]
itens (x:y:xs)
  | x == ' ' = itens (y:xs)
  | x /= ' ' && y == ' ' = (read ([x]) :: Int) : itens xs
  | otherwise = (read ([x] ++ [y]) :: Int) : itens xs

appRegra :: Int -> [Int] -> Provas -> Result Term
appRegra 1 [a] p = do (c,t) <- return $ termCont [] $ p !! ((length p) - a)
                      case curryTerm c (Fst t) of
                        Right r -> return $ (either (\_ -> error "Parser deu Errado") id (runp (show r)))
                        Left _ -> throwError $ "Não é possivel usar regra de eliminção do AND no termo " ++ (show a)
appRegra 2 [a] p = do (c,t) <- return $ termCont [] $ p !! ((length p) - a)
                      case curryTerm c (Snd t) of
                        Right r -> return $ (either (\_ -> error "Parser deu Errado") id (runp (show r)))
                        Left _ -> throwError $ "Não é possivel usar regra de eliminção do AND no termo " ++ (show a)
appRegra 4 [a,b] p = do (c1,t1) <- return $ termCont [] $ p !! ((length p) - a)
                        (c2,t2) <- return $ termCont [] $ p !! ((length p) - b)
                        case curryTerm (c2 ++ c1) (t1 :@: t2) of
                          Right r -> return $ (either (\_ -> error "Parser deu Errado") id (runp (show r)))
                          Left _ -> throwError $ "Não é possivel usar regra de eliminção do AND no termo " ++ (show a)

process :: State -> IO ()
process s@(i,p,o,pr) = do putStr $(show i) ++ ":"
                          x <- getLine
                          putStrLn $ show (regras x)
                          case regras x of
                            (0, xs) -> fim o (pr++p) s
                            (-1, xs) -> putStrLn "Regra não reconhecida." >> (process s)
                            (n, xs) -> do x <- return $ either (\_ -> error "Não foi possível usar a regra") id $ appRegra n (itens xs) (pr++p)
                                          process (i+1,p,o,x:pr)

main :: IO ()
main = premissa
