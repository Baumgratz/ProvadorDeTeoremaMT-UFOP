module Shell where

import Parser
import Term
import Tipagem
import Control.Monad.Except

type Premissa = [Term]
type Objetivo = Tipo
type Provas = [Term]
type Result a = Either String a

type State = (Int, Premissa, Objetivo, Provas, Int)


premissa :: State -> IO (State)
premissa (np,pre,obj,seqts,pst)
   = do putStr "1:"
        x <- getLine
        case (words x) of
          "Objetivo:":t:_ -> either (\_ -> error "Parser deu Errado")
                                    (\(trm,nwpst) -> return (np,pre, tObj trm, seqts, nwpst) )
                                    (runp t 0)
          xs -> (either (\_ -> error "Parser deu Errado")
                        (\(trm,nwpst) -> premissa (np,pre++[trm], obj, seqts, nwpst) )
                        (runp (unwords xs) pst))

tObj :: Term -> Tipo
tObj (t1 ::: t2) = t2

fim' :: Objetivo -> Provas -> Bool
fim' a [] = False
fim' a ((t1 ::: t2):xs) = (a == t2) || (fim' a xs)

fim :: Objetivo -> Provas -> State -> IO (State)
fim o p s
  | fim' o p = putStrLn "Foi provado!" >> return s
  | otherwise = process s

process :: State -> IO (State)
process s@(i,p,o,pr,pst) = do putStr $(show i) ++ ":"
                              x <- getLine
                              case words x of
                                "cqe":xs -> fim o  s
                                "E&e":t1:t2:"->":t3 -> undefined


main :: IO ()
main = premissa (0,[],error "Sem objetivo",[],0) >> return ()
