module Shell where

import Parser
import Term
import Tipagem
import Control.Monad.Except
import Data.List
import Types

premissa :: State -> IO (State)
premissa s@(np,pre,obj,pst,ctx,sup)
   = do putStr $ (show np)++":"
        x <- getLine
        case (words x) of
          "objetivo:":[]  -> do putStrLn " Erro: objetivo não pode ser vazio"
                                premissa s
          "objetivo:":ys  -> either (\_ -> putStrLn "Termo inválido. Digite um termo correto." >> premissa s)
                                    (\tipo -> return (np,pre, tipo, pst, preCont pre,sup) )
                                    (runt (unwords ys))
          xs -> loadTerm s (unwords xs) >>= (\s -> premissa s)

preCont :: Provas -> Contexto
preCont [] = []
preCont (x:xs) = c ++ preCont xs
  where
    c = termCont [] x

loadTerm :: State -> String -> IO (State)
loadTerm s@(np,pre,obj,pst,ctx,sup) ys
  = either (\_ -> return s)
           (\t -> (return (deepInst s t)) >>= (\(t,(np,pre,obj,pst,ctx,sup)) -> return (np+1, pre++[t],obj,pst,ctx,sup)))
           (runt ys)

tObj :: Term -> Tipo
tObj (t1 ::: t2) = t2

fim :: Objetivo -> Term -> State -> IO (State)
fim o f s@(_,_,_,_,ctx,sup) = case (ti,sup) of
                                (Right _,0) -> return s
                                _ -> process s
  where
    ti = curryTerm ctx (f:::o)

int :: String -> Int
int s = read s :: Int

newTipo :: String  -> IO Tipo
newTipo ys = either (\_ -> putStrLn "Termo inválido. Digite um termo correto." >> return TFalse )
                    (\t -> return t)
                    (runt ys)

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
elimI p1 p2 ys s@(np,pre,obj,pst,ctx,sup) = do tipo <- newTipo ys
                                               case tipo of
                                                 TFalse -> return s
                                                 ti -> case curryTerm ctx (t1 :@: t2 ::: (ti)) of
                                                         Right _ -> return (np+1, pre ++[t1 :@: t2 ::: (ti)],obj,pst,ctx,sup)
                                                         Left a -> putStrLn "Não é possivel reduzir para o termo desejado." >> return s
  where
    t1 = pre !! (p1-1)
    t2 = pre !! (p2-1)

intrE :: Int -> Int -> String -> State -> IO State
intrE p1 p2 ys s@(np,pre,obj,pst,ctx,sup) = do tipo <- newTipo ys
                                               case tipo of
                                                 TFalse -> return s
                                                 ti -> case curryTerm ctx (t1 :*: t2 ::: (ti)) of
                                                         Right _ -> return (np+1, pre ++[t1 :*: t2 ::: (ti)],obj,pst,ctx,sup)
                                                         Left a -> putStrLn "Não é possivel reduzir para o termo desejado." >> return s
  where
    t1 = pre !! (p1-1)
    t2 = pre !! (p2-1)


intrO :: Int -> Int -> String -> State -> IO State
intrO 1 p ys s@(np,pre,obj,pst,ctx,sup) = do tipo <- newTipo ys
                                             case tipo of
                                                TFalse -> return s
                                                ti -> case curryTerm ctx (TLeft t  ::: (ti)) of
                                                        Right _ -> return (np+1, pre ++[TLeft t ::: ti],obj,pst,ctx,sup)
                                                        Left _ -> putStrLn "Não é possivel reduzir para o termo desejado." >> return s
  where
    t = pre !! (p-1)
intrO 2 p ys s@(np,pre,obj,pst,ctx,sup) = do tipo <- newTipo ys
                                             case tipo of
                                                TFalse -> return s
                                                ti -> case curryTerm ctx (TRight t  ::: ti) of
                                                        Right _ -> return (np+1, pre ++[TRight t ::: ti],obj,pst,ctx,sup)
                                                        Left _ -> putStrLn "Não é possivel reduzir para o termo desejado." >> return s
  where
    t = pre !! (p-1)

elimO :: Int -> Int -> Int -> String -> State -> IO State
elimO p1 p2 p3 ys s@(np,pre,obj,pst,ctx,sup) = do tipo <- newTipo ys
                                                  case tipo of
                                                    TFalse -> return s
                                                    ti -> case curryTerm ctx (TEither t1 t2 t3 ::: ti) of
                                                            Right _ -> return (np+1, pre ++[TEither t1 t2 t3 ::: ti],obj,pst,ctx,sup)
                                                            Left _ -> putStrLn "Não é possivel reduzir para o termo desejado." >> return s

  where
    t1 = pre !! (p1-1)
    t2 = pre !! (p2-1)
    t3 = pre !! (p3-1)



intrI :: Int -> Int -> String -> State -> IO State
intrI p1 p2 ys s@(np,pre,obj,pst,ctx,sup) = do tipo <- newTipo ys
                                               case (tipo,t1) of
                                                 ((ti1 :>: ti2),(V a):::b) -> case curryTerm ctx (LamT a b t2 ::: tipo) of
                                                                                Right _ -> return (delete (a,b) ctx) >>= (\c -> return (np+1,pre++[(LamT a b t2 ::: tipo)],obj,pst,c,sup-1))
                                                                                Left _ -> putStrLn "Não foi possível reduzir para o termo desejado" >> return s
                                                 _ -> return s
  where
    t1 = pre!!(p1-1)
    t2 = pre!!(p2-1)

process :: State -> IO (State)
process s@(np,pre,obj,pst,ctx,sup) = do putStr $(show np) ++ ":"
                                        x <- getLine
                                        -- putStrLn (show s)
                                        -- a<- newTipo $ last $ words x
                                        -- putStrLn (show a)
                                        case words x of
                                          "cqe":xs -> fim obj (last pre) s
                                          "E&e":t:"=>":ti -> (elimE 1 (int t) (unwords ti) s) >>= (\s -> process s)
                                          "E&d":t:"=>":ti -> (elimE 2 (int t) (unwords ti) s) >>= (\s -> process s)
                                          "suponha:":t -> either (\_ -> putStrLn "Termo invalido. Digite um termo correto." >> process s)
                                                                 (\t -> return (varInst s t) >>= (\(t,(np,pre, obj, pst, ctx,sup)) -> process (np+1,pre++[t], obj, pst, termCont ctx t,sup+1) ))
                                                                 (runt (unwords t))
                                          "E->":t1:t2:"=>":ti -> elimI (int t1) (int t2) (unwords ti) s >>= (\s -> process s)
                                          "I&":t1:t2:"=>":ti -> intrE (int t1) (int t2) (unwords ti) s >>= (\s -> process s)
                                          "I|e":t:"=>":ti -> intrO 1 (int t) (unwords ti) s >>= (\s -> process s)
                                          "I|d":t:"=>":ti -> intrO 2 (int t) (unwords ti) s >>= (\s -> process s)
                                          "I->":t1:t2:"=>":ti -> intrI (int t1) (int t2) (unwords ti) s >>= (\s -> process s)
                                          "id":t -> return (pre !!((int (unwords t)) -1)) >>= (\t -> process (np,pre++[t], obj, pst, ctx,sup))
                                          "E|":t1:t2:t3:"=>":tf -> elimO (int t1) (int t2) (int t3) (unwords tf) s >>= (\s -> process s)
                                          _ -> putStrLn ("Comando não reconhecido.") >> process s

main :: IO ()
main = do s <- premissa (1,[],error "Sem objetivo",0,[],0)
          -- putStrLn (show s)
          process s
          -- putStrLn $ show s
          putStrLn "Foi Provado"
