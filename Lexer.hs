module Lexer (
Token(..),
Symbol_T(..),
Operator_T(..),
Keyword_T(..),
lexi
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


{-
    FUNZIONE TOSYMBOL:

    -- converto i simboli nei corrispoettivi TOKEN
-}
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
    


{-

   DESCRIZIONE FUNZIONI:

   Funzioni che implementano direttamente gli stati dell'automa. Osserva che
   non c'è ricorsione. Il passaggio dallo stato iniziale e principale I ad un
   altro stato è realizzato con un'invocazione. Poi si ritorna sempre a I e
   quindi basta il normale ritorno della funzione.
-}


{-
   FUNZIONE N:
   -- n input numero segno
   -- n è la stringa in input
   -- numero è il numero elaborato finora
   -- segno è il segno del numero, true sse è negativo (rilevato da I)
   -- Stato N per riconoscere i numeri
-}
n :: String -> Integer -> Bool -> (Token, String)
n "" _ _ = error "Unexpected end of string"
n input@(c:l) num sign
    | isDigitChar c =
        let d = read [c] :: Integer
        in n l (num*10 + d) sign
    | otherwise = (Number((if sign then -1 else 1) * num), input)


{-
    FUNZIONE SC:

   -- sc input stringa
   -- stringa è la stringa elaborata finora
   -- Stato SC per riconoscere le stringhe tra virgolette
-}
sc :: String -> String -> (Token, String)
sc "" _ = error "Unexpected end of string"
sc ('"':l) res = (String res, l)
sc (c:l) res = sc l (res ++ [c])


{-
    FUNZIONE S:

   -- s input stringa
   -- stringa è l'identificatore elaborato finora
   -- Stato S per raccogliere le stringhe che possono corrispondere ad identificatori, operatori prefissi o keyword
-}
s :: String -> String -> (Token, String)
s "" _ = error "Unexpected end of string"
s input@(c:l) res
    | isIdChar c = s l (res ++ [c])
    | otherwise = (extractWord(res), input)



{-
    FUNZIONE PRINTER:

   --funzione di utilita' che permette di stampare qualsiasi input, visto che e' considerato stringa
-}
printer :: String -> String
printer input = show(input)

{-
    FUNZIONE SUBPARSER:

    -- f::Char : è la prima parte dell'input raw che riceve da i
    -- l::[Char] : è la seconda parte dell'input raw che ricevo da i
    -- raw::[String]: è l'input originale originale
    -- casi:
        se '\"' sta cominciando una stringa, invoco sc passando l (il passo succ) e "" ( sono all'inizio )
        se '~'  sta per arrivare un numero negativo passo l a n (0 perchè sono all'inizio) e TRUE perchè negativo
        se nor  sta per arrivare un carattere char o un numero (positivo quindi FALSE) quindi invoco n/s
-}
subparser :: Char -> [Char] -> String -> (Token, String)
subparser '\"' l raw = sc l ""
subparser '~' l raw = n l 0 True
subparser f l raw
    | isDigitChar f = n raw 0 False
    | isAlphaChar f = s raw ""
subparser  _ _ _    = error ("Something is wrong during parsing")    



{-

    LEXER:

    L'analizzatore lessicale riceve come input un programma in LispKit, cioè una lista
    di caratteri e deve riconoscere le componenti elementari del linguaggio e deve met-
    terle in una forma che sia semplice da manipolare nella successiva fase di analisi
    sintattica ==> TOKEN

    Per esempio, deve riconoscere le costanti (per esempio i numeri interi
    oppure il valore true, eccetera), le parole chiave, gli identificatori, gli operatori ed
    i simboli di separazione.
-}

{-
    FUNZIONE i:

    -- input::[String] ricevo la stringa in LispKit
    -- f::Char è la prima parte della stringa
    -- l::[Char] è la parte rimanente
    -- faccio la ricorsione sui paramentri successivi e al ritorno pongo il risultato
    -- uso subparser per la gesione dei casi "strani"

-}
i :: String -> [Token]
i "" = error "Unexpected end of string"
i "$" = [(Symbol DOLLAR)]
i input@(f:l)
  | isSpace  f = i l
  | isSymbol f = (Symbol (toSymbol f)) : i l
  | otherwise =
    let 
      (token, next_input) = subparser f l input
    in token : i next_input


{-
    FUNZIONE LEXI:

    -- Funzione principale per l'analisi lessicale
-}
lexi :: String -> [Token]
lexi = i






























