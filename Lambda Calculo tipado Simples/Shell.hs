module Shell where

import Parser
import Term
import Tipagem
import Control.Monad.Except

type Objetivo = Tipo
type Provas = [Term]
type Result a = Either String a
type Contexto = [(Var, Tipo)]
type Suponha = [Term]
type State = (Int, Provas, Objetivo, Int, Contexto, Suponha)

premissa :: State -> IO (State)
premissa s@(np,pre,obj,pst,ctx,sup)
   = do putStr $ (show np)++":"
        x <- getLine
        case (words x) of
          "objetivo:":[]  -> do putStrLn " Erro: objetivo não pode ser vazio"
                                premissa s
          "objetivo:":ys  -> either (\_ -> putStrLn "Termo inválido. Digite um termo correto." >> premissa s)
                                    (\(trm,nwpst) -> return (np,pre, tObj trm, nwpst, preCont pre,sup) )
                                    (runp (unwords ys) pst)
          xs -> loadTerm s (unwords xs) >>= (\s -> premissa s)

preCont :: Provas -> Contexto
preCont [] = []
preCont (x:xs) = c ++ preCont xs
  where
    (c,_) = termCont [] x

loadTerm :: State -> String -> IO (State)
loadTerm s@(np,pre,obj,pst,ctx,sup) ys
  = either (\_ -> putStrLn "Termo inválido. Digite um termo correto." >> return s)
           (\(trm,nwpst) -> return ((np+1),pre++[trm], obj, nwpst, ctx,sup) )
           (runp ys pst)

tObj :: Term -> Tipo
tObj (t1 ::: t2) = t2

fim' :: Objetivo -> Provas -> Bool
fim' a [] = False
fim' a ((t1 ::: t2):xs) = (a == t2) || (fim' a xs)

fim :: Objetivo -> Term -> State -> IO (State)
fim o f s@(_,_,_,_,ctx,_) = case ti of
                              Right _ -> putStrLn "Foi provado!" >> return s
                              Left _ -> process s
  where
    ti = curryTerm ctx (f:::o)

int :: String -> Int
int s = read s :: Int

newTipo :: String  -> IO Tipo
newTipo ys = either (\_ -> putStrLn "Termo inválido. Digite um termo correto." >> return TFalse )
                    (\(trm,nwpst) -> return (tObj trm) )
                    (runp ys 0)

elimE :: Int -> Int -> String -> State -> IO State
elimE 1 p ys s@(np,pre,obj,pst,ctx,sup) = do  tipo <- newTipo ys
                                              case tipo of
                                                TFalse -> return s
                                                ti -> case curryTerm ctx (Snd (pre !! (p-1)) ::: tipo) of
                                                        Right _ -> return (np+1, pre ++[Snd (pre !! (p-1)) ::: tipo],obj,pst,ctx,sup)
                                                        Left _ -> putStrLn "Não é possivel reduzir para o termo desejado." >> return s
elimE 2 p ys s@(np,pre,obj,pst,ctx,sup) = do  tipo <- newTipo ys
                                              case tipo of
                                                TFalse -> return s
                                                ti -> case curryTerm ctx (Fst (pre !! (p-1)) ::: tipo) of
                                                        Right _ -> return (np+1, pre ++[Fst (pre !! (p-1)) ::: tipo],obj,pst,ctx,sup)
                                                        Left _ -> putStrLn "Não é possivel reduzir para o termo desejado." >> return s

elimI :: Int -> Int -> String -> State -> IO State
elimI p1 p2 ys s@(np,pre,obj,pst,ctx,sup) = undefined

process :: State -> IO (State)
process s@(np,pre,obj,pst,ctx,sup) = do putStr $(show np) ++ ":"
                                        x <- getLine
                                        putStrLn (show s)
                                        case words x of
                                          "cqe":xs -> fim obj (last pre) s
                                          "E&e":t:"=>":ti -> (elimE 1 (int t) (unwords ti) s) >>= (\s -> process s)
                                          "E&d":t:"=>":ti -> (elimE 2 (int t) (unwords ti) s) >>= (\s -> process s)
                                          "Suponha:":t -> either (\_ -> putStrLn "Termo inválido. Digite um termo correto." >> return s)
                                                                 (\(trm,nwpst) -> return (np,pre, obj, nwpst, ctx,sup++[trm]) )
                                                                 (runp (unwords t) pst)

main :: IO ()
main = do s <- premissa (1,[],error "Sem objetivo",0,[],[])
          putStrLn (show s)
          process s
          putStrLn $ show s
