--Compilatore LKC --> SECD incompleto Dicembre  2015
module Compilatore(
comp_one,
Secdexpr(..)
)
where

import Analizzatore







{-

    DataType per la macchina SEDC

    -- La macchina SEDC compie queste azioni interpretandole
    -- Sub, Mul, Div, Rem, Eq, Leq, Car, Cdr, Cons, Atom, Join, Rtn, Ap, Stop, Rap, Push
    -- Sono tutte operazioni 0-arie che utilizzano gli operandi presenti nello stack
    -- Queste operazioni 0-arie non hanno operandi (mentre in LKC li hanno). Il motivo è che nella macchina SECD,
       gli operandi di questi operatori vengono messi in cima allo stack S (attraverso operazioni Ld (load normale *) o Ldc (load costante **), prima che venga eseguito
       l’operatore che li usa. Il risultato dell’operazione viene poi inserito in cima allo stack S della SECD al posto degli operandi appena usati.
    -- STOP server fare lo stop
    -- PUSH e RAP servono per la gestione delle funzioni ricorsive (LETRECC)
    -- LD (n1:Integer, n2:Integer): carica in cima alla pila S l'n2-esimo valore della n1-esima cella di E (*)
    -- LDC LKC: carica in cima alla pila un valore LKC costante (NUM, BOO, STRI, NIL) (**)
    -- SEL [Secdexpr] [Secdexpr]: compilazione di un if, le due espressioni sono i due rami THEN ELSE
                                  se la situazione della SECD è la seguente:

                                    ((T S) E  ((Sel C_T C_F) C) D --> S E C_T (C D)

                                  Qquindi il programmma C che segue Sel viene memorizzato sul dump D { ((Sel C_T C_F) C) D }  viene
                                  eseguito C_T infatti ==> sullo stack S c'è T { (T S) } cioè T == TRUE. altrimenti con F ==> C_F

                                  Alla fine devo avere la JOIN che fa il restore dal DUMP del CONTROLLO C cosi : (***)

                                    S E  (Join) (C D) --> S E C D

    -- LDF [Secdexpr]: costruisce in cima alla pila S la chiusura di una funzione
    -- JOIN : S E (Join) (C D)-->S E C D ==> restore del CONTROLLO C dal DUMP (***)

-}
data Secdexpr = Add | Sub |  Mult | Div | Rem | Eq | Leq | 
                Car | Cdr | Cons | Atom | Join | Rtn | Stop | Push |
                Ap | Rap | Ld (Integer, Integer) |
                Ldc LKC|
                Sel [Secdexpr] [Secdexpr]|
                Ldf [Secdexpr]
                deriving(Show, Eq)


{-

    FUNZIONE POSITION:

    -- calcola l'indirizzo di una variabile all'interno dell'ambiente
    --  String: nome della variabile
    --  [LKC]: ambiente in cui cercare


-}
position::String -> [LKC]-> Integer
position x [] = error "position: non trova la variabile"
position x ((VAR z):y) = if z==x then 0 else 1 + (position x y)
position x _ = error "position: trovata non VAR"


{-
    FUNZION MEMBER:

    --  verifica se la variabile compare all'interno del RA; se non la trovo vado avanti in modo ricorsivo
        se alla fine del RA non la trovo allora ==> ERRORE

    --  Stirng: Nome della variabile
    --  [LKC]: Record di attivaziome

-}
member::String ->[LKC]->Bool
member x  [] = False 
member x ((VAR z):y) = if x == z then True else member x y
member x _ = error ("member: trovata non VAR"++ x)

{-
    FUNZIONE LOCATION:

    -- ritorna l'indirizzo (n1,n2) della variabile X in N
    -- successivamente l'istruzione Ld (n1, n2) carichera R-valore della variabile x sullo stack
    -- prende in input Var 'x' e restituisce la posizione in E
    --  String x: è il nome delle variabile
    --  Integer ct: è il contatore dei record passati, alla prima invocazione è 0
    --  [[LKC]] (n:m): è lo stack

    -- In sostanza nel caso "corretto" prendo scorro tutti gli stack da 0 a i e poi faccio una ricorsione sul RA k (MEMBER X N)
       se dentro lo stack k trovo la variabile X allora ne calcolo la posizione (POSITION  X N) altrimenti passo al RA k +1
       k alla fine corrisponderà al ct

-}
--            x       0         n           n(n1,n2)
location:: String->Integer-> [[LKC]]-> (Integer, Integer) 
location x _ []= error ("location non trova VAR"++ x)
location x ct (n:m) =   if (member x n) then (ct, (position x n)) else  (location x (ct+1) m)




{-
    FUNZIONE SEXPR_REVERSE:

    -- E' senza tipo quindi è polimorfismo parametrico, anche se posso forzare io il tipo a mano
    -- inverte una lista ritornando una lista la cui coda diventa la testa e viceversa
-}
sexpr_reverse::[a]->[a]
sexpr_reverse []= []
sexpr_reverse (a:b)= (sexpr_reverse b) ++ [a]


{-
    FUNZIONE VARS:

    -- toglie variabili da una lista di Binders
    -- La lista in ingresso è fatta cosi: [(VAR x, NUM 1), [(.... ]
    -- La lista in uscita è fatta cosi:  [VAR "x",VAR "y",...] (tengo solo l'x)

-}

vars::[(a,b)]->[a]
vars []= [] 
vars ((x,y):r)= (x : (vars r))


{-
    FUNZIONE EXPRS:

    -- toglie espressioni da una lista di Binders
    -- La lista in ingresso è fatta cosi: [(VAR x, NUM 1), [(.... ]
    -- La lista in uscita cosi: [NUM 1,NUM 2,...] ( tengo solo l' y )

-}
exprs::[(a,b)]->[b]
exprs []= [] 
exprs((x,y):r)= (y:(exprs r))


{-
    FUNZIONE COMPLIST:

    -- serve per preparare i parametri formali di una funzione ==> compila i parametri attuali!

    -- fa una ricorsione su tutti i parametri attuali, compila ciascun parametro dal primo all'ultimo
       e li mette in una lista per permettere alla funzione chiamata di utilizzarli come parametri
       attuali (cioè quelli passati all'invocazione)

    -- Se la lista dei parametri è vuota ( ricorsione o nessun par ) compila NIL. e lo mette prima di c
    -- a questo punto dopo, viene fatto l' LD del parametro/i
    -- deve essere fatto prima di AP altrimenti invoco la funzione in modo sbagliato

    --  pa: [LKC] lista dei parametri da compilare
    --  n:  [[LKC]] ambiente statico
    --  c:  parte già compilata


    -- (Cons:c) viene eseguito quando in cima alla pila c'è l'ultimo parametro attuale, esegue quindi la concatenazione
       producendo un'unica lista di parametri ( è al contrario )
    -- (Comp x n ...) produce il codice per calcolare il parametro attuale e mettere il risultato in cima alla pila

    -- complist y n ... fa l'invocazione ricorsiva e di conseguenza l'ordine è invertito

        es: y = [p1,p2]
        passo 1: (comp p1) (cons) c
        passo 2: (comp p2) (cons) (comp p1) (cons) c
        passo 3: (Ldc NIL) (comp p2) (cons) (comp p1) (cons) c

    -- cons è l'istruzione SECD che concatena una lista Può essere che un parametro attuale sia una definzione lambda (un funzione),
       in questo caso nella lista dei parametri viene messa l'istruzione (Ldf ...) La lista sarà quindi qualcosa tipo:
       [(Ldc NIL), (Ldf ...), (Cons), (Ld (n1,n2)), (Cons)]

    -- Quando il codice viene eseguito carica nello stack la chiusura della funzione,
        la concatena a NIL, mette in cima allo stack il valore della variabile ed esegue un
        altro Cons per produrre la lista dei valori da usare per invocare la funzione.
        La differenza sta che i parametri attuali che sono valori, vengono semplicemente caricati
        mentre per quanto riguarda quelli funzionali viene compialto il codice e prodotta la chiusura.
        Da notare che il codice compilato non viene eseguito.
-}
--          pa      n           c
complist:: [LKC]-> [[LKC]] -> [Secdexpr]->[Secdexpr]
complist [] _ c = ((Ldc NIL):c)
complist (x:y) n c = complist y n (comp x n (Cons:c))


{-
        FUNZIONE COMP:

        -- Il codice viene prodotto da destra a sinistra e viene eseguito da sinistra a destra
        -- e::[LKC] istruzione LKC che devo tradurre
        -- n::[[LKC]] AMBIENTE STATICO oambiente statico, ovvero la lista di liste di nomi di variabili che sono presenti quando viene compilato
                        In sostanza è lo stack del programma fino al passo e ==> all'inzio per forza di cose è []
        -- c::[Secdexpr]: codice SEDC finora compilato fino a quel momento ==> all'inizio per forza di cose è []
            le istruzioni vengono aggiunte in testa alla lista
        -- istr::[Secdexpr]: è l'istruzione generata

        L'interprete chiama cosi ==> (S=[], E=[], C=COMP(e,[],[]), D=[]).


-}
--      e        n           c       istr SECD
comp:: LKC -> [[LKC]] -> [Secdexpr]->[Secdexpr]

                        {-
                            LOAD:
                            -- faccio il load della variabili, prendendo dalla liste n1 il valore n2
                            -- LOAD(n1, n2)

                            VAR:
                            -- carica sullo stack il valore della variabile

                            NUM:
                            -- carica sullo stack il valore constante

                            ADD & SUB:
                            -- produce [CodicePerY][CodicePerX][ADD][c] in questo modo quando viene eseguita l'add e
                               in cima alla pila rimane il valore di X
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

                            LAMBADC x y:

                            -- x sono i parametri formali della funzione
                            -- Il compilatore quindi produce l'istruzione per creare la chiusura della funzione (Ldf)
                            -- METTO su S la chiusura della funzione che è definita

                            -- Il codice della funzione viene ottenuto compilando y in un nuovo ambiente statico contente i parametri formali. (****)
                            -- Il codice prodotto termina con un Rtn per far riprendere l'esecuzione del programma

                            -- Creo una chiusura di funzione con ambiente statico n e la lista dei parametri formali. (****)

                            -- Metto i parametri formali in n perche cosi quado la funzione viene invocata i suoi parametri
                               attuali saranno nello stesso ordine dei parametri formali (creati a tempo di compilazione) in cima all'ambiente dinamico E.

                            -- LAMBDAC [LKC] LKC  x = parm.formali y = corpo funz. i param formali vanno sull'ambiente statico

                            -- COMP produce codice SECD corrispondente al corpo body usando un ambiente statico n a cui viene aggiunta all’inizio la lista
                               dei parametri formali parform della funzione. Il motivo di aggiungere ad n i
                               parametri formali è che ogni volta che questa funzione verrà invocata (durante
                               l'esecuzione del programma SECD), i suoi parametri attuali saranno preventivamente
                               messi in cima all’ambiente dinamico E (cf. istruzione Ap della Sezione 2)
                               ed occuperanno quindi la stessa posizione che i loro nomi occupano nell’ambiente
                               statico quando il corpo della funzione `e compilato.

                            -- Rtn esegue il ritorno, cioè toglie dal dump D la prima tripla (S E C)
                               e la usa per inizializzare, rispettivamente, la pila, l’ambiente dinamico e il controllo
                               della macchina SECD. Intuitivamente, in questo modo si ripristina la situazione della macchina prima dell’invocazione
                               e l’effetto dell’invocazione `e costituito unicamente dal suo risultato che si troverà in cima alla pila S

                            -- Allora, B sarà parametro di un'istruzione Ldf la quale, quando
                               verr`a eseguita, caricher`a B sullo stack S, costruendo, allo stesso tempo, la
                               chiusura (B,E), dove E `e l’ambiente dinamico al momento dell’esecuzione di
                               Ldf, cio`e al momento della definizione della funzione stessa. Questo ambiente
                               E serve a realizzare lo scope statico

                               LDF ===> CREA CHIUSURA IN CIMA A S ==> COPPIA CORPO | AMBIENTE ===> BINDING STATICO HO TUTTE LE VARIABILI LI


                         -}
                        (LAMBDAC x y)-> (Ldf (comp y (x:n) [Rtn])):c

                        {-
                               -- è sensato che si comporti come una CALL perchè alla fine è una call, è l'inzio del programma e quindi la gestisco come una chiamata di funzione
                        -}
                        (LETC x y)->  let
                                            var     =   vars y -- definizioni dei binders (nomi)
                                            expr    =   exprs y -- espressioni per il valore dei binders
                                            {-

                                            LETC x y:
                                            -- x è il corpo della funzione
                                            -- preparo prima i parametri delle expr
                                            -- simile a CALL preparo i parametri del programma grazi a complist
                                            -- uso AP per caircare il corpo della funzione sul controllo e di costruire l'ambiente in cui fare l'esecuzione
                                            -- genera un ambiente E' in cui in testa metto var
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



                                             -- risultato:
                                                [codice per la lista dei parametri attuali (parte destra dei binders), prodotta con complist][(Ldf body)][(Ap)]

                                             -- Da notare che l'ambiente statico quando viene eseguita la funzione deve essere ((binders):n), dove binders è il record definito dalla valutazione della lista.
                                                mentre durante la compilazione della parte destra dei binders l'ambiente deve essere solo n.

                                             -- x è il codice della parte IN
                                             -- viene fatta con il record delle variabil in cima il all'ambiente
                                             -- dinamico

                                            -}
                                       in
                                             complist expr n ((Ldf (comp x (var:n) [Rtn])):Ap:c)
                                             -- complist expr n produce il codice per calcolare il valore dei binders
                                             -- Ldf costruisce la chiusura per la parte IN (codice prodotto da comp x..)
                                             -- Ap esegue la chiusura che si trova in cima alla pila
                                             -- c è il codice finora prodotto.

                                             -- compilo i parametri attuali ( preparazione formali) --tipo call
                                             -- metto in cima ad  (ambiente statico)
                                             -- compilo l'ambiente dinamico e il corpo e metto var sopra n statico
                                             -- AP di quello che ho in cima ad S

                        {-
                           -- Push inserisce il record  OGA
                           -- complist e var:n produce il codice per calcolare il valore dei binders, l'ambiente statico di complist contiene in cima anche il nome dei binders,
                                questo perché le espressioni dei binders possono essere delle definizioni di funzioni ricorsive
                                e di conseguenza devono avere a disposizione anche il loro nome che è contenuto in v.
                           -- La presenza di var nell'ambiente statico porta un problema nel calcolo degli indirizzi, perché viene considerato un RA in più, che a runtime non è presente.
                              Per questo motivo viene prima eseguita una PUSH (che non modifica l'ambiente statico) la quale aggiunge un RA fittizzio che rappresenta l'RA var dell'ambiente dinamico.
                           -- Ldf costruisce la chiusura per la parte IN (codice prodotto da   comp x..)
                           -- Ap esegue la chiusura che si trova in cima alla pila
                           -- c è il codice finora prodotto.
                        -}
                        (LETRECC x y)-> let

                                            var     =   vars y -- definizioni dei binders (nomi)

                                            expr    =   exprs y -- espressioni per il valore dei binders

                                         in
                                            Push : (complist expr (var:n) ((Ldf (comp x (var:n) [Rtn])):Rap:c))


                        {-


                            --(nom,param_att)->prep param form (invocazione funz)
                            -- y rappresenta i parametri attuali
                            -- x è il nome della funzione da invocare
                            -- (complist y n ...) compila le istruzioni per il calcolo dei parametri attuali
                            -- (comp x n ...) compila l'istruzione che carica in cima allo stack la chiusura della funzione x
                            -- Ap : c ==> salva su D la situazione corrente di SEDC e invoca la funzione, in cima allo stack deve esserci una chiusura con sotto la lista dei parametri attual

                        -}
                        (CALL x y)-> complist y n (comp x n (Ap:c))
                        _ -> [];

 
--esempi di prova



{-

    comp_one "let x=6 and LOL = lambda(f t) f  in (LOL (2,3)) * 5 end $"
    [Ldc NIL,Ldf [Ld (0,0),Rtn],Cons,Ldc (NUM 6),Cons,Ldf [Ldc (NUM 5),Ldc NIL,Ldc (NUM 3),Cons,Ldc (NUM 2),Cons,Ld (0,1),Ap,Mult,Rtn],Ap]
    -- come parametri di let ho : NIL, corpo di LOL, 6
    -- parametri compilati cosi per essere in ordine con i parametri formali
    -- per IN:
        compilo 5, load del parametro 1 di let ( la funzione LOL ) unisco con i suoi parametri attuali ( 2 e 3)
        e aggiungo il NIL ( as ever )

    -- I suoi parametri attuali saranno preventivamente
       messi in cima all’ambiente dinamico E (cf. istruzione Ap della Sezione 2)
       ed occuperanno quindi la stessa posizione che i loro nomi occupano nell’ambiente
       statico quando il corpo della funzione `e compilato.

-}

comp_one x = comp (generateLKCFromLispKit(x)) [] []