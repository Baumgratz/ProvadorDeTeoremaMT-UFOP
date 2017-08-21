module Shell where

import Parser
import Term
import Tipagem

type Premissa = [Term]
type Objetivo = Term
type Provas = [Term]

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

premissa :: IO State
premissa = do x <- getLine
              case elemObj x of
                True -> do y <- return $ noObj x
                           process (0,[], either (\_ -> error "Parser deu Errado") id (runp y),[])
                False -> premissa' 0 [either (\_ -> error "Parser deu Errado") id (runp x)]

premissa' :: Int -> [Term] -> IO State
premissa' i p = do x <- getLine
                   case (elemObj x) of
                      True -> do y <- return $ noObj x
                                 process (i,p, either (\_ -> error "Parser deu Errado") id (runp y),[])
                      False -> premissa' (i+1) $ (either (\_ -> error "Parser deu Errado") id (runp x)):p

regras :: String -> (Int, String)
regras (x:y:z:xs)
  | 'E' == x = regrasE (y:z:xs)
  | 'I' == x = regrasI (y:z:xs)
  | 'c' == x = (0, xs)
  | otherwise = (-1, x:xs)

regrasE :: String -> (Int, String)
regrasE w@(x:y:z:xs)
  | x == '&' = (1, y:z:xs)
  | x == '|' = (2, y:z:xs)
  | x == '-' = (3, z:xs)
  | otherwise = (-1, w)

regrasI :: String -> (Int, String)
regrasI w@(x:y:z:xs)
  | x == '&' = (4, y:z:xs)
  | x == '|' = (5, y:z:xs)
  | x == '-' = (6, z:xs)
  | otherwise = (-1, w)

process :: State -> IO (State)
process (i,p,o,pr) = do x <- getLine
                        case regras x of
                          (1, xs) -> undefined
                          (2, xs) -> undefined
                          (3, xs) -> undefined
                          (4, xs) -> undefined
                          (5, xs) -> undefined
                          (6, xs) -> undefined
                          (-1, xs) -> undefined
                          (0, xs) -> undefined

main :: IO ()
main = undefined
