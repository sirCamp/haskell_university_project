module Lexer (
Token(..),
Symbol_T(..),
Operator_T(..),
Keyword_T(..),
lexi,
c,
d
) where

import Prelude hiding (EQ)
import Data.Typeable

-- Tipi

data Keyword_T = LET | IN | END | LETREC | AND | IF | THEN | ELSE | LAMBDA
    deriving (Show,Eq)

data Operator_T = EQ | LEQ | CAR | CDR | CONS | ATOM
    deriving (Show,Eq)

data Symbol_T = LPAREN | RPAREN | EQUALS | PLUS | MINUS | TIMES | DIVISION |VIRGOLA| DOLLAR
    deriving (Show,Eq)

data Token = Keyword Keyword_T | Operator Operator_T | Id String |
    Symbol Symbol_T | Number Integer | String String | Bool Bool | Nil
    deriving (Show,Eq)



-- Funzioni di supporto

-- Testa se il carattere è un carattere valido per iniziare un identificatore, un operatore o una keyword
isAlphaChar c = c `elem` (['a' .. 'z'] ++ ['A' .. 'Z'])

-- Riconosce se c è un carattere numerico o no
isDigitChar c = c `elem` ['0' .. '9']

-- Testa se il carattere è un carattere valido per comporre un identificatore, un operatore o una keyword (ad eccezione del primo carattere)
isIdChar c = isAlphaChar c || isDigitChar c

-- Testa se il carattere è un separatore
isSeparator c = c `elem` "()=$,"

-- Testa se è uno spazio o accapo
isSpace c = c `elem` [' ', '\n', '\f', '\r', '\t']

isSymbol c = c `elem` "()=+-*/,"


{- data una stringa X la confronta con le parole chiavi e con gli operatori
   del Lisp Kit e se è una di queste cose, restituisce la corrispondente
   coppia token_lexema, altrimenti la considera un identificatore e
   restituisce la coppia (ID, STRINGA(X)) -}
extractWord :: String -> Token
extractWord w = case w of
    "let"     -> Keyword LET
    "in"      -> Keyword IN
    "end"     -> Keyword END
    "letrec"  -> Keyword LETREC
    "and"     -> Keyword AND
    "if"      -> Keyword IF
    "then"    -> Keyword THEN
    "else"    -> Keyword ELSE
    "lambda"  -> Keyword LAMBDA
    
    "eq"      -> Operator EQ
    "leq"     -> Operator LEQ
    "car"     -> Operator CAR
    "cdr"     -> Operator CDR
    "cons"    -> Operator CONS
    "atom"    -> Operator ATOM
    
    "true"    -> Bool True
    "false"   -> Bool False
    
    "nil"     -> Nil
    
    otherwise -> Id w

toSymbol :: Char -> Symbol_T
toSymbol c = case c of
    '(' -> LPAREN
    ')' -> RPAREN
    '+' -> PLUS
    '-' -> MINUS
    '*' -> TIMES
    '/' -> DIVISION
    '=' -> EQUALS
    ',' -> VIRGOLA
    


{- Funzioni che implementano direttamente gli stati dell'automa. Osserva che
   non c'è ricorsione. Il passaggio dallo stato iniziale e principale I ad un
   altro stato è realizzato con un'invocazione. Poi si ritorna sempre a I e
   quindi basta il normale ritorno della funzione. -}

-- Stato N per riconoscere i numeri
{- n input numero segno
   n è la stringa in input
   numero è il numero elaborato finora
   segno è il segno del numero, true sse è negativo (rilevato da I) -}
n :: String -> Integer -> Bool -> (Token, String)
n "" _ _ = error "Unexpected end of string"
n input@(c:l) num sign
    | isDigitChar c =
        let d = read [c] :: Integer
        in n l (num*10 + d) sign
    | otherwise = (Number((if sign then -1 else 1) * num), input)

-- Stato SC per riconoscere le stringhe tra virgolette
{- sc input stringa
   stringa è la stringa elaborata finora -}
sc :: String -> String -> (Token, String)
sc "" _ = error "Unexpected end of string"
sc ('"':l) res = (String res, l)
sc (c:l) res = sc l (res ++ [c])

-- Stato S per raccogliere le stringhe che possono corrispondere ad identificatori, operatori prefissi o keyword
{- s input stringa
   stringa è l'identificatore elaborato finora -}
s :: String -> String -> (Token, String)
s "" _ = error "Unexpected end of string"
s input@(c:l) res
    | isIdChar c = s l (res ++ [c])
    | otherwise = (extractWord(res), input)



--Funzione di utilita' che permette di stampare qualsiasi input, visto che e' considerato stringa

printer :: String -> String
printer input = show(input)

subparser :: Char -> [Char] -> String -> (Token, String)
subparser '\"' l raw = sc l ""
subparser '~' l raw = n l 0 True
subparser f l raw
    | isDigitChar f = n raw 0 False
    | isAlphaChar f = s raw ""
subparser  _ _ _    = error ("Something is wrong during parsing")    


-- FUNZIONE i DA FARE
i :: String -> [Token]
i "" = error "Unexpected end of string"
i "$" = [(Symbol DOLLAR)]
 -- i (' ':l) = i l  ## inrealta non serve, perchè incluso in ogni caso
i input@(f:l)
  | isSpace  f = i l
  | isSymbol f = (Symbol (toSymbol f)) : i l           --posso usare il $
  | otherwise =
    let 
      (token, next_input) = subparser f l input
    in token : i next_input


-- Funzione principale per l'analisi lessicale
lexi :: String -> [Token]
lexi = i

c = "letrec  FACT = lambda ( X ) if  eq ( X, 0 ) then 1 else  X*FACT(  X- 1 )and G = lambda ( H L ) if  eq ( L,  nil ) then L else cons( H(car( L ) ), G ( H, cdr ( L ) )) in G ( FACT, cons(1, cons(2, cons(3, nil))) ) end $";

d = "let x=cons(\"ab\", cons(\"cd\", nil)) in if true then cons(\"01\", x) else nil end $";




-- il dollaro mette prima tutto
































