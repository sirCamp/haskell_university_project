--Compilatore LKC --> SECD incompleto Dicembre  2015
module Compilatore(
comp_one,
Secdexpr(..)
)
where

import Analizzatore



data Secdexpr = Add | Sub |  Mult | Div | Rem | Eq | Leq | 
                Car | Cdr | Cons | Atom | Join | Rtn | Stop | Push |
                Ap | Rap | Ld (Integer, Integer) |
                Ldc LKC|
                Sel [Secdexpr] [Secdexpr]|
                Ldf [Secdexpr]
                deriving(Show, Eq)


-- funzioni per il calcolo dell'indirizzo di una variabile nell'ambiente 
position::String -> [LKC]-> Integer
position x [] = error "position: non trova la variabile"
position x ((VAR z):y) = if z==x then 0 else 1 + (position x y)
position x _ = error "position: trovata non VAR"

member::String ->[LKC]->Bool
member x  [] = False 
member x ((VAR z):y) = if x == z then True else member x y
member x _ = error ("member: trovata non VAR"++ x)

{-
    FUNZIONE LOCATION
    -- calcola l'indirizo (n1,n2) della variabile X in N
    -- successivamente l'istruzione Ld (n1, n2) carichera R-valore della variabile x sullo stack
    -- prende in input Var 'x' e restituisce la posizione in E

-}
--            x       0         n           n(n1,n2)
location:: String->Integer-> [[LKC]]-> (Integer, Integer) 
location x _ []= error ("location non trova VAR"++ x)
location x ct (n:m) =   if (member x n) then (ct, (position x n)) else  (location x (ct+1) m)
 

{-
    FUNZIONE EXPR_REVERSE
    -- inverte una lista ritornando una lista la cui coda diventa la testa e viceversa
-}
sexpr_reverse::[a]->[a]
sexpr_reverse []= []
sexpr_reverse (a:b)= (sexpr_reverse b) ++ [a]


{-
    FUNZIONE VARS
    -- toglie variabili da una lista di Binders
    -- La lista e fatta cosi: [(VAR x, NUM 1).... ]
-}

vars::[(a,b)]->[a]
vars []= [] 
vars ((x,y):r)= (x : (vars r))


{-
    FUNZIONE EXPRS
    -- toglie espressioni da una lista di Binders
    -- La lista e fatta cosi: [(VAR x, NUM 1).... ]
-}
exprs::[(a,b)]->[b]
exprs []= [] 
exprs((x,y):r)= (y:(exprs r))


{-
    FUNZIONE COMPLIST
    -- serve pre preparare i parametri attuali di una funzione
    -- fa una ricorsione su tutti i parametri attuali, compila ciascun parametro dal primo all'ultimo
        e li mette in una lista per permettere alla funzione chiamata di utilizzarli come parametri
        attuali (cioè quelli passati allp'invocazione)

    -- Se la lista dei parametri è vuota ( ricorsione o nessun par ) compila NIL.
    -- a questo punto dopo, viene fatto l' LD del parametro/i
    -- deve essere fatto prima di AP altrimenti invoco la funzione in modo sbagliato

-}
--          pa      n           c
complist:: [LKC]-> [[LKC]] -> [Secdexpr]->[Secdexpr]
complist [] _ c = ((Ldc NIL):c) 
complist (x:y) n c = complist y n (comp x n (Cons:c))


{-
        FUNZIONE COMP
        e = PROGRAMMA LKC che devo tradurre
        n = AMBIENTE STATICO ovvero la lista delle varibili che sono presenti quando viene compilato
            In sostanza è lo stack del programma fino al passo e ==> all'inzio per forza di cose è []
        c = CONTIENE IL CODICE SECD prodotto fino a quel momento ==> all'inizio per forza di cose è []
            le istruzioni vengono aggiunte in testa alla lista
        istr SECD = è l'istruzione generata

        L'interprete chiama cosi ==> (S=[], E=[], C=COMP(e,[],[]), D=[]).

-}
--      e        n           c       istr SECD
comp:: LKC -> [[LKC]] -> [Secdexpr]->[Secdexpr]

                        {-
                            LOAD
                            -- faccio il load della variabili, prendendo dalla liste n1 il valore n2
                            -- LOAD(n1, n2)
                        -}
comp e n c =  case e of (VAR x) -> ((Ld (location x 0 n)):c)
                        (NUM x)-> (Ldc (NUM x)):c 
                        (BOO x)-> (Ldc (BOO x)):c  
                        (STRI x)-> (Ldc (STRI x)):c 
                        NIL -> (Ldc NIL):c

                        {-
                            OPERAZIONI
                            -- Carico l'operazione da eseguire
                            -- CArico il primo operando
                            -- Carico il secondo operando
                        -}
                        (ADD x y)-> comp y n (comp x n (Add:c))
                        (SUB x y)-> comp y n (comp x n (Sub:c))
                        (MULT x y)-> comp y n (comp x n (Mult:c))
                        (DIV x y)-> comp y n (comp x n (Div:c))
                        (REM x y)-> comp y n (comp x n (Rem:c))
                        (EQC x y)-> comp y n (comp x n (Eq:c))
                        (LEQC x y)-> comp y n (comp x n (Leq:c))
                        (CARC x)-> comp x n (Car:c)  
                        (CDRC x)-> comp x n (Cdr:c)  
                        (CONSC x y)-> comp y n (comp x n (Cons:c))  
                        (ATOMC x)-> comp x n (Atom:c)
                        {-
                            FUNZIONE SEL
                            -- secexpr secexpr
                            -- se x vero lancio y con i suoi parametri n
                            -- se x falso lancio z con i suoi parametri n
                            -- alla fine facco la join che mi fa ripristinare dal DUMP
                                il codice da mettere in C, questo garantisce l'esecuzione dell'istruzione successiva
                        -}
                        (IFC x y z)-> let thenp=(comp y n [Join]) 
                                          elsep=(comp z n [Join]) 
                                      in comp x n  ((Sel thenp elsep):c)

                        {-

                            -- METTO su S la chiusura della funzione che è definita
                            -- Creo una chiusura di funzione con ambiente statico n e la lista dei parametri formali.
                            -- Metto i parametri formali in n perche cosi quado la funzione viene invocata i suoi parametri
                               attuali saranno nello stesso ordine dei parametri formali (creati a tempo di compilazione) in cima all'ambiente dinamico E.
                            -- LAMBDAC [LKC] LKC  x = parm.formali y = corpo funz. i param formali vanno sull'ambiente statico
                            -- COMP produce codice SECD corrispondente al
                               corpo body usando un ambiente statico n a cui viene aggiunta all’inizio la lista
                               dei parametri formali parform della funzione. Il motivo di aggiungere ad n i
                               parametri formali `e che ogni volta che questa funzione verr`a invocata (durante
                               l’esecuzione del programma SECD), i suoi parametri attuali saranno preventivamente
                               messi in cima all’ambiente dinamico E (cf. istruzione Ap della Sezione 2)
                               ed occuperanno quindi la stessa posizione che i loro nomi occupano nell’ambiente
                               statico quando il corpo della funzione `e compilato.
                            -- Rtn esegue il ritorno, cioè toglie dal dump D la prima tripla (S E C)
                               e la usa per inizializzare, rispettivamente, la pila, l’ambiente dinamico e il controllo
                               della macchina SECD. Intuitivamente, in questo modo si ripristina la situazione della macchina prima dell’invocazione
                               e l’effetto dell’invocazione `e costituito unicamente dal suo risultato che si troverà in cima alla pila S
                            -- Allora, B sar`a parametro di un’istruzione Ldf la quale, quando
                               verr`a eseguita, caricher`a B sullo stack S, costruendo, allo stesso tempo, la
                               chiusura (B,E), dove E `e l’ambiente dinamico al momento dell’esecuzione di
                               Ldf, cio`e al momento della definizione della funzione stessa. Questo ambiente
                               E serve a realizzare lo scope statico

                         -}
                        (LAMBDAC x y)-> (Ldf (comp y (x:n) [Rtn])):c

                        {-
                               -- è sensato che si comporti come una CALL perchè alla fine è una call, è l'inzio del programma e quindi la gestisco come una chiamata di funzione
                        -}
                        (LETC x y)->  let
                                            var     =   vars y
                                            expr    =   exprs y
                                            {-
                                            -- preparo prima i parametri delle expr
                                            -- simile a CALL preparo i parametri del programma grazi a complist
                                            -- uso AP per caircare il corpo della funzione sul controllo e di costruire l'ambiente in cui fare l'esecuzione
                                            -- genera un ambiente E'
                                            -- con let eseguo subito la funzione
                                            -- Infatti una LETC introduce binders della forma x=exp, in
                                                cui le variabili locali x, per il corpo del LETC, sono del tutto simili ai parametri
                                                formali del caso LAMBDA. Quindi il corpo del LETC va compilato mettendo queste
                                                variabili locali in cima all’ambiente statico n usato dal compilatore. D’altra
                                                parte i valori delle variabili locali, cio`e i valori dei corrispondenti exp, hanno
                                                lo stesso ruolo dei parametri attuali di una CALL e quindi, in corrispondenza di
                                                questi valori, va prodotto codice SECD che costruisca una lista di questi valori
                                                sullo stack S. Sopra questa lista dovr`a venire inserita la chiusura del corpo del
                                                LETC e in questa situazione, una Ap far`a eseguire il corpo del LETC nell’ambiente
                                                dinamico corretto (con i valori delle variabili locali disponibili ed al posto
                                                giusto, cio`e in cima all’ambiente dinamico).
                                            -}
                                       in
                                            complist expr n ((Ldf (comp x (var:n) [Rtn])):Ap:c)


                                        
                        (LETRECC x y)-> let

                                            var     =   vars y
                                            expr    =   exprs y
                                         in
                                            Push : (complist expr (var:n) ((Ldf (comp x (var:n) [Rtn])):Rap:c))

                        {-
                            --(nom,param_att)->prep param form (invocazione funz)
                            y : parametri formali della funzione, li passo a complist che me li prepara
                            x : nome dei parametri
                            Ap : c ==> salva su D la situazione corrente di SEDC
                        -}
                        (CALL x y)-> complist y n (comp x n (Ap:c))
                        _ -> [];

 
--esempi di prova

--c="letrec  FACT = lambda ( X ) if  eq ( X, 0 ) then 1 else  X*FACT(  X - 1 ) and G = lambda ( H L ) if  eq ( nil, L ) then L else c--ons( H(car( L ) ), G ( H, cdr ( L ) )) in G ( FACT, cons(1 ,cons(2, cons(3, nil))) ) end $"



--d= "let x= 5 and y= 6 in x*3 + y * 2* x + x*y end $"

comp_one x = comp (generateLKCFromLispKit(x)) [] []