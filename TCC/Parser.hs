module Parser (runStart)where

import TermType
import Text.Parsec
import Data.Functor.Identity

type LParser = Parsec String [(String, Int)]

updateD :: [(String, Int)] -> [(String, Int)]
updateD (a:xs) = map (\(a,b) -> (a,b-1)) xs

updateU :: String ->  [(String, Int)]  ->  [(String, Int)]
updateU s ys = (s,0):(map (\(x,y) -> (x,y+1)) ys)

varfree :: String -> TermInf
varfree s = Free $ Global s

addTipo :: TermInf -> TermInf -> TermInf
addTipo (Free (Global x)) s = Pi (Inf (varfree x)) (Inf s)
-- addTipo (Star) s = Pi (Inf Star) (Inf s)
addTipo (Pi a b@(Inf (Free _))) s = Pi a $ Inf $ Pi b (Inf s)
-- addTipo (Pi a b@(Inf Star)) s = Pi a $ Inf $ Pi b (Inf s)
addTipo (Pi a (Inf b)) s = Pi a $ Inf $ addTipo b s

parensT :: LParser TermInf
parensT = spaces >> (
         do char '('
            t1 <- tipo
            spaces
            char ')'
            parensT' t1
         )

parensT' :: TermInf -> LParser TermInf
parensT' t1 = spaces >> (try(
              do string "->"
                 t2 <- tipo
                 return (Pi (Inf t1) (Inf t2))
             )<|>(return t1)
             )

tipo :: LParser TermInf
tipo = spaces >> (try(
       do x <- many1 (alphaNum <|> oneOf "!@#$%")
          tipo' (varfree x)
       )<|>
       (do p <- parensT
           tipo' p)
       )

tipo' :: TermInf -> LParser TermInf
tipo' s = spaces >> (try(
          do string "->"
             spaces
             x <- many1 (alphaNum <|> oneOf "!@#$%")
             tipo' (addTipo s (varfree x))
          )<|>
          try(
           do string "->"
              p <- parensT
              tipo' (addTipo s p)
           )<|>
          (return s)
          )

parserI :: LParser TermInf
parserI = spaces >> (
          do l <- (lam <|> (app >>= return.Inf))
             spaces
             string "::"
             t <- tipo
             return (Ann l (Inf t))
             )

app :: LParser TermInf
app = spaces >> (try(
      do xs <- termName
         app' xs
      )<|>try(
      do p <- parens app
         app' p
      )<|>(
      do p <- parens parserI
         app' p
      )
      )


app' :: TermInf -> LParser TermInf
app' t = spaces >> (try(
         do xs <- termName
            app' (t :@: Inf xs)
         )<|>
         try(do l <- lam
                app' (t :@: l)
         )
         <|>
         try(do p <- parens lam
                app' (t :@: p)
         )
         <|>(return t)
         )

lam :: LParser TermCheck
lam = spaces >> (try(
      do char '\\'
         spaces
         n <- many1 (alphaNum <|> oneOf "!@#$%")
         modifyState (updateU n)
         spaces
         char '.'
         l <- lam
         modifyState (updateD)
         return (Lam l)
      )<|>
      (app >>= return.Inf)
      )

parens :: LParser a -> LParser a
parens t = spaces >> (
            do char '('
               p <- t
               spaces
               char ')'
               return p
           )

start :: LParser TermCheck
start = spaces >> (try (parserI>>=return.Inf) <|> lam)

termName :: LParser TermInf
termName = do s <- many1 (alphaNum <|> oneOf "!@#$%")
              t <- getState
              case lookup s t of
                 Just i  -> (return (Bound i))
                 Nothing -> (return $ varfree s)

runner :: LParser a -> SourceName -> String -> Either ParseError a
runner p sn s = runParser p [] sn s

run :: LParser a -> String -> Either ParseError a
run p s = runner p "" s

runStart :: String -> Either ParseError TermCheck
runStart = run start
