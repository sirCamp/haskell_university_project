module Analizzatore(
progdoll,
generateLKCFromLispKit,
LKC(..)
)
where
import Lexer
import Prelude hiding (EQ,exp)

data Exc a = Raise Exception | Return a
type Exception = String

instance Show a => Show (Exc a) where
 show (Raise e)= "ERRORE:" ++ e
 show (Return x) = "RAGGIUNTO:" ++ (show x)
 
 {-
 
 instance Functor Exc where
  fmap f (Raise e) = Raise e 
  fmap f (Return v) = Return (f v)

instance Applicative Exc where  
    pure = Return  
    (Raise e) <*> _ = Raise e
    (Return f) <*> something = fmap f something
    
-}    

instance Monad Exc where
 return x  = Return x
 (Raise e) >>= q   = Raise e
 (Return x) >>= q  = q x


{-
    LKC
-}
data LKC
  = ETY  -- epsilon
  | VAR String
  | NUM Integer
  | STRI String
  | BOO Bool
  | NIL
  | ADD LKC LKC
  | SUB LKC LKC
  | MULT LKC LKC
  | REM LKC LKC
  | DIV LKC LKC
  | EQC LKC LKC
  | LEQC LKC LKC
  | CARC LKC
  | CDRC LKC
  | CONSC LKC LKC
  | ATOMC LKC
  | IFC LKC LKC LKC
  | LAMBDAC [LKC] LKC
  | CALL LKC [LKC]
  | LETC LKC [(LKC,LKC)]
  | LETRECC LKC [(LKC, LKC)]
    deriving(Show, Eq)

raise :: Exception -> Exc a
raise e = Raise e


{-
    FUNZIONE REC_KEY
    -- in questo modo ritorno il costruttore corretto LETC o  LETRECC in base alla key
    -- è currificato
    -- tipo dei costruttori: (LKC -> [(LKC, LKC)] -> LKC) ( è come se ritornassi una funzione )
-}
rec_key :: [Token] -> Exc ([Token], (LKC -> [(LKC, LKC)] -> LKC))
rec_key ((Keyword LET):b)    = Return (b,LETC)
rec_key ((Keyword LETREC):b) = Return (b,LETRECC)
rec_key (a:b)                = Raise ("trovato " ++ show(a) ++", atteso LET o LETREC")
rec_key  x                   = Raise ("ERRORE STRANO"  ++  show(x))

rec_in::[Token]->Exc[Token]
rec_in ((Keyword IN):b)= Return b
rec_in (a:b)           = Raise ("trovato " ++ show(a) ++ ", atteso IN")

rec_end::[Token]->Exc[Token]
rec_end ((Keyword END):b)= Return b
rec_end (a:b)            = Raise ("trovato " ++ show(a) ++ ", atteso END")


rec_then ((Keyword THEN):b)= Return b
rec_then (a:b)             = Raise ("trovato " ++ show(a) ++ ", atteso THEN")


rec_else ((Keyword ELSE):b)= Return b
rec_else (a:b)             = Raise ("trovato " ++ show(a) ++ ", atteso ELSE")


rec_lp ((Symbol LPAREN):b)=Return b
rec_lp (a:b)              = Raise ("trovato " ++ show(a) ++ ", atteso ELSE")


rec_rp ((Symbol RPAREN):b)=Return b
rec_rp (a:b)              = Raise ("trovato " ++ show(a) ++ ", in"++show(b) ++" attesa )")



rec_virg ((Symbol VIRGOLA):b)=Return  b
rec_virg (a:b)               = Raise ("trovato " ++ show(a) ++ ", attesa ,")



rec_equals ((Symbol EQUALS):b)= Return b
rec_equals (a:b)              = Raise ("trovato " ++ show(a) ++ ", atteso =")



progdoll::[Token] -> String
progdoll x= show (prog x)

{-
    FUNZIONE PROG:

    -- funzione che rappresenta le produzioni del non terminale Prog
    -- aspetto il costruttore currificato LETC / LETRECC
    -- per il resto sono uguali, basta creare gli LKC corretti
    -- ​1.1:   Prog​ ::= let Bind in Exp end
    -- 1.2:   Prog​ ::= letrec Bind in Exp end

    -- LETC LKC [(LKC,LKC)]    -- LETC Corpo [(var,exp)]
    -- LETRECC LKC [(LKC,LKC)] -- LETRECC Corpo [(var,exp)]
    -- Ho il costruttore currificato che mi arriva da rec_key

    -- cerco LET | LETREC
    -- cerco produzione BIND e traduco
    -- cerco Token IN
    -- cerca produzione EXP e traduco
    -- cerco Token END
-}
prog:: [Token] -> Exc([Token],LKC)
prog a =
    do
        (x, trad_letc_letrecc)<-rec_key a
        (y, trad_bind) <- bind x
        z    <- rec_in y
        (w, trad_exp) <- exp z
        k <- rec_end w
        Return (k, (trad_letc_letrecc trad_exp trad_bind))

{-
    FUNZIONE EXP:

    -- rappresento il non terminale EXP

    -- LET elaboro il blocco LET
    -- LETREC elaboro il blocco LETREC
    -- LAMBDA trad_sec_var  trad_exp ==> LAMBDAC argomenti e corpo
-}
exp::[Token]->Exc([Token],LKC)
exp a@((Keyword LET):b)    = (prog a)
exp a@((Keyword LETREC):b) = (prog a)
exp ((Keyword LAMBDA):b)   = do
                                x     <-rec_lp b
                                (z, trad_sec_var) <- seq_var x --argomenti
                                (w, trad_exp)  <- exp z
                                Return(w, (  LAMBDAC trad_sec_var  trad_exp ))
exp ((Operator CONS):b)    = do
                                x<-rec_lp b
                                (y, first_trad) <-exp x
                                z<-rec_virg y
                                (w, new_trad) <-exp z
                                result <- rec_rp w
                                Return(result,  CONSC first_trad  new_trad)
exp ((Operator LEQ):b)     = do
                                x<-rec_lp b
                                (y, first_trad) <-exp x
                                z<-rec_virg y
                                (w, new_trad) <- exp z
                                result <- rec_rp w
                                Return(result, ( LEQC first_trad  new_trad))
exp ((Operator EQ):b)      = do
                                x<-rec_lp b
                                (y, first_trad) <-exp x
                                z<- rec_virg y
                                (w, new_trad) <-exp z
                                result<- rec_rp w
                                Return(result, ( EQC  first_trad  new_trad ))
exp ((Operator CAR):b)      = do
                                (result, trad) <- exp b
                                Return (result, ( CARC  trad))
exp ((Operator CDR):b)      =
                                do
                                (result, trad) <- exp b
                                Return (result, ( CDRC  trad))
exp ((Operator ATOM):b)     =
                                do
                                (result, trad) <- exp b
                                Return (result, ( ATOMC  trad))
exp ((Keyword IF):b)        = do
                                (x, trad_exp_if) <- exp b
                                y<-rec_then x
                                (z, trad_exp_then) <-exp y
                                w<-rec_else z
                                (result, trad_exp_else ) <- exp w
                                Return(result, (IFC  trad_exp_if trad_exp_then trad_exp_else ))
exp x                       =  expa x



{-
    FUNZIONE BIND:

    -- i due attributi devono assumere i seguenti
       valori: Trad(Bind) deve assumere come valore la lista delle coppie [(Var
       ''x1'', LKC(Exp1)),...,(Var ‘‘xn’’, LKC(Expn))]:LKC*LKC list

    -- richiamo bind continuamente funche inizio a ritornare quindi tirono LKC[EXP]
       se seguo la sezione qui dovrei avere un assegnazione variabile valore
       quindi ho [variabile nomervaribile, EXP successiva (val)] cioè quello che fa il bind

    -- (Id a) è il token che rappresenta il nome del bind
    -- 2) Bind​ ::= var = Exp X
    -- Bisogna produrre:
    -- [(VAR ‘‘x’’,NUM 5),(VAR ‘‘y’’,NUM 6)]

    -- cerco il Token EQUALS
    -- cerco l'EXP ==> valore
    --


-}
bind:: [Token]->Exc([Token],[(LKC,LKC)])
bind ((Id a):b)            =  do
                                x<- rec_equals b
                                (y,variable_value) <- exp x -- scendo sull'albero
                                (next_tok, next_variable_value) <- funx y
                                Return(next_tok, (lister(VAR a, variable_value) ++ next_variable_value))

bind (a:_)                  = Raise ("BINDER CON "++ show(a) ++" A SINISTRA")


{-
    FUNZIONE FUNX:

    -- non terminale X
    -- elaboro il binder successivo
    -- per colpa di bind b ora ritorno una lista di coppie di LKC quindi non posso
       ritornare solo a, ma siccome sono sun IN è il primo colpo dove definisco l'albero, ergo, ritorno vuoto
-}
funx ((Keyword AND):b)     = bind b
funx a@((Keyword IN):b)    = Return (a,[])
funx (a:_)                 = Raise ("DOPO BINDERS; TROVATO"++show(a))


{-
    FUNZIONE EXPA:

    -- rappresenta il non terminale EXPA

-}
expa a = do
           (x, trad_funt)<- funt a
           fune1 x trad_funt


{-
    FUNZIONE FUNT
    - Sono obbligato a chiamare T1 passandogli dietro anche le traduzioni fatte fino ad ora
    --funt :: [Token]-> Exc([Token],LKC)
-}


funt a = do
           (x, trad_funt)<-funf a
           funt1 x trad_funt



{-
    FUNZIONE FUNE1

    -- il tipo inposto dal fatto che h attributi ereditati
    --  ADD LKC LKC
    --  SUB LKC LKC

    -- "let x = 5 and y = 4 in y + x end $" ==> diventa ...(ADD (VAR y) (VAR x))....
        in sostanza VAR y è l'elemento che ho ereditato prima di elaborare VAR x
        la stessa cosa si può applicare ad altri operatori

-}
fune1 :: [Token] -> LKC -> Exc([Token],LKC)
fune1 ((Symbol PLUS):b)  operand  =
                                     do
                                        (x, trad_funt) <- funt b
                                        (y, trad_fune1) <- fune1 x trad_funt
                                        Return(y, ADD operand trad_fune1)

fune1 ((Symbol MINUS):b) operand  =
                                    do
                                        (x,trad_funt) <-funt b
                                        (y, trad_fune1) <- fune1 x trad_funt
                                        Return(y, SUB operand trad_fune1)

fune1 x operand                     = Return(x, operand) -- follow perchè ho epsilon in first

{-
    FUNZIONE FUNT1:

    -- il tipo inposto dal fatto che h attributi ereditati
    --  MULT LKC LKC
    --  DIV LKC LKC

    ATT:
    -- devo stare attento a non ritornare subito, perchè altrimenti aggiungo un MULT prima
    -- funt1 x (MULT operand trad_funf) --> no sbaglio
    -- stessa cosa per la DIV
    -- es LETC (ADD (MULT (VAR "x") (NUM 3)) (ADD (MULT (VAR "y") (DIV (NUM 2) (VAR "x"))) (MULT (VAR "x") (VAR "y")))) [(VAR "x",NUM 5),(VAR "y",NUM 6)]
        ho il DIV tra 2 e X che deve essere fatto prima di MULT
-}
funt1 :: [Token] -> LKC ->Exc([Token],LKC)
funt1 ((Symbol TIMES):b) operand  =
                                do
                                    (x, trad_funf) <-funf b
                                    (z, trad_funt1) <- funt1 x trad_funf
                                    Return(z, MULT operand trad_funt1)

funt1 ((Symbol DIVISION):b) operand
                                = do
                                    (x, trad_funf) <-funf b
                                    (z, trad_funt1) <- funt1 x trad_funf
                                    Return(z,(DIV operand trad_funt1))
funt1 x  operand                = Return(x, operand)



{-
    FUNZIONE FUNF:

    -- produzioni di F
    -- NUM Integer, BOO Bool, STRI String, NIL
    -- se TRUE ritorno la costante LKC con token relativo
    -- se FALSE ritorno Fx --> controllo di non essere alla fine

-}
funf (a:b)                     =
                                do
                                    result <- exp_const a
                                    case result of
                                        (True, lkc) ->  Return(b,lkc)
                                        (False, empty) -> do
                                                            (x, lkc) <- fX(a:b)
                                                            Return(x, lkc)


{-
    FUNZIONE FX:

    -- controllo di non essere alla fine, se non sono alla fine allora invoco la funzione con CALL
    --  VAR LKC se viene referenziato il nome di una variabile
    --  CALL LKC [LKC] se dopo il token Id c'è una sequenza di espressioni
-}
fX:: [Token] ->Exc([Token],LKC)
fX ((Id a):b)              =  do
                                (x, trad_funy) <- fuy b
                                if(trad_funy == [ETY])
                                    then Return(x, VAR a)
                                    else Return(x, CALL (VAR a) trad_funy)
-- espressione algebrica
fX ((Symbol LPAREN):b)     = do
                              (x, trad_expa)<- expa b
                              z <- rec_rp x
                              Return (z, trad_expa)
fX (a:_)                   = Raise ("ERRORE in fX, TROVATO "++ show(a))


{-
    FUNZIONE EXP_CONST
-}
exp_const::Token ->Exc(Bool,LKC)
exp_const (Number val)  =  Return (True,NUM val)
exp_const Nil           =  Return (True, NIL)
exp_const (Bool val)    = Return (True, BOO val)
exp_const (String val)  = Return (True, STRI val)
exp_const  _            = Return (False, ETY)


{-
    FUNZIONE FUY
    -- sono obbligato a cambiare il tipo di ritono ( visto il valore di x )
    -- il secondo parametro du fuy è epsilon quindi vuoto --> ritono ETY
    -- lista di LKC perchè sono parametri di funzione
-}
fuy :: [Token]-> Exc([Token],[LKC])
fuy ((Symbol LPAREN):b)      =  do
                                 (x, trads) <- seq_exp b
                                 y <- rec_rp x
                                 Return(y, trads)
fuy x                        =   Return (x, lister(ETY))



{-
    FUNZIONE DI SUPPORTO
    -- polimorfismo param qualsiasi tipo lo metto in una lista in quanto NON dichiaro il tipo

-}

lister a = [a]


{-
    FUNZIONE N
    -- nuova proposizione inserita
    -- caso in cui dopo la virgola ho un altra espressione ( il before intendo la ricorsione precedente )
    -- sono al mio ultimo valore ( nel follow non ho altro ) quindi inzio a ritornare la 'lista' dei Token e del LKC

-}
n :: [Token] -> Exc([Token], [LKC])
n a @ (Symbol VIRGOLA : l) =
                            do
                            (before, first_trad)  <- seq_exp l
                            Return(before, first_trad)

n a @ (Symbol RPAREN : _ ) =  Return (a,[])
n a @ ( some : _ ) = Raise("ERRORE, ci deve essere qualcosa dopo " ++show(some) )
n _ =  Raise("ERRORE, la sinstassi non e corretta")


{-
    FUNZIONE SEQ_VAR
    -- ID o altro tipo
    -- tolgo la parentesi destra all'ultima iterazione
    -- nel first ho "epsilon"  ==> devo considerare il follow
       ( in sostanza se sono vuoto devo considerare il primo dei miei successori )

-}
seq_var:: [Token]-> Exc([Token], [LKC])
seq_var a @ (Id id : l) =
                        do
                         (before, first_trad)   <- seq_var l
                         Return(before, ((lister(VAR id)) ++ first_trad))

seq_var a @ (Symbol RPAREN : l ) = Return ( l, [] )
seq_var a @ (some : _) = Raise ("ERRORE in seq_var, TROVATO "++ show(some))
seq_var _ =  Raise("ERRORE, la sinstassi non e corretta")

{-
    FUNZIONE SEQ_EXP
    -- se ho la combo messa cosi allora ritorno

-}
seq_exp:: [Token]-> Exc([Token], [LKC])
seq_exp a @ (Symbol RPAREN : _) = Return(a, [] )
seq_exp a =
    do
        (next, next_trad) <- exp a
        (result, last_trad) <- n next
        Return (result, (lister(next_trad) ++ last_trad ))



{-
    FUNZIONE generateLKC
    -- mi ritorna LKC da una Exc(Token,LKC)
-}
generateLKC :: Exc ([Token],LKC) -> LKC
generateLKC (Return(a,b)) = b
generateLKC (Raise errore) = error("C'è stato un errore"++errore)

{-
    FUNZIONE generateLKC
    -- mi ritorna LKC da una Stringa di LispKit
-}
generateLKCFromLispKit :: String -> LKC
generateLKCFromLispKit s =  case s of
                                "" -> error ("Devi inserire un programma")
                                t ->  generateLKC(prog((lexi s)))




{-
    FIRST determina l’insieme di simboli terminali che possono cominciare una stringa di simboli, FOLLOW
    determina l’insieme di simboli terminali che possono seguire un simbolo non terminale in una
    derivazione in G

    sappiamo che i token cosi hanno un unica corrispondenza nelle produzioni ==> una sola entry


    ambigua e = e +e | e* e | t ==> e + (e *e) | (e+e) | * e ==> serve precedenza
-}
