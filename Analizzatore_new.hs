module Analizzatore(
progdoll,
d,
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

rec_key::[Token]-> Exc [Token]
rec_key ((Keyword LET):b)    = Return b
rec_key ((Keyword LETREC):b) = Return b 
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
rec_rp (a:b)              = Raise ("trovato " ++ show(a) ++ ", attesa )")



rec_virg ((Symbol VIRGOLA):b)=Return  b 
rec_virg (a:b)               = Raise ("trovato " ++ show(a) ++ ", attesa ,")



rec_equals ((Symbol EQUALS):b)= Return b 
rec_equals (a:b)              = Raise ("trovato " ++ show(a) ++ ", atteso =")



progdoll::[Token] -> String
progdoll x= show (prog x)
             
prog:: [Token] -> Exc([Token],[LKC])
prog a@(first, remain)

    | first == Keyword LET = Return([Keyword LET],lister(LETC))
    | first == Keyword LETREC = Return([Keyword LETREC],lister(LETRECC))
    | otherwise =
        do
             x<-rec_key a
             y<-bind x
             z<-rec_in y
             (w, trads) <-exp z
             rec_end w
             Return(w, trads)

 
exp::[Token]->Exc([Token],[LKC])
exp a@((Keyword LET):b)    = (prog a)
exp a@((Keyword LETREC):b) = (prog a)
exp ((Keyword LAMBDA):b)   = do
                                (x, first_trad) <- seq_var b
                                (result, new_trad) <- exp x
                                Return(result, ( (lister LAMBDAC) ++ (first_trad ++ new_trad )))
exp ((Operator CONS):b)    = do
                                x<-rec_lp b
                                (y, first_trad) <-exp x
                                z<-rec_virg y
                                (w, new_trad) <-exp z
                                rec_rp w
                                Return(w, ((lister CONSC)  ++ first_trad ++ new_trad))
exp ((Operator LEQ):b)     = do
                                x<-rec_lp b
                                (y, first_trad) <-exp x
                                z<-rec_virg y
                                (w, new_trad) <- exp z
                                rec_rp w
                                Return(w, ((lister LEQC)  ++ first_trad ++ new_trad))
exp ((Operator EQ):b)      = do
                                x<-rec_lp b
                                (y, first_trad) <-exp x
                                z<- rec_virg y
                                (w, new_trad) <-exp z
                                rec_rp w
                                Return(w, ((lister EQC) ++ first_trad ++ new_trad)) -- la parentesi va prima, ovviamente
exp ((Operator CAR):b)      =
                                do
                                (result, trad) <- exp b
                                Return (result, ((lister CARC ) ++ trad))
exp ((Operator CDR):b)      =
                                do
                                (result, trad) <- exp b
                                Return (result, ((lister CDRC ) ++ trad))
exp ((Operator ATOM):b)     =
                                do
                                (result, trad) <- exp b
                                Return (result, ((lister ATOMC ) ++ trad))
exp ((Keyword IF):b)        = do
                                (x, first_trad) <- exp b
                                y<-rec_then x
                                (z, second_trad) <-exp y
                                w<-rec_else z
                                (result, last_trad ) <- exp w
                                Return(result ((lister IFC) ++ first_trad ++ second_trad ++ last_trad ))
exp x                       =  expa x


bind ((Id a):b)            =  do
                                x<- rec_equals b
                                y<- exp x
                                funx y
bind (a:_)                  = Raise ("BINDER CON "++ show(a) ++" A SINISTRA")

funx ((Keyword AND):b)     = bind b
funx a@((Keyword IN):b)    = Return a
funx (a:_)                 = Raise ("DOPO BINDERS; TROVATO"++show(a))



expa a = do
           x<- funt a
           fune1 x


funt a = do
           x<-funf a
           funt1 x



fune1 ((Symbol PLUS):b)    = do
                              x<- funt b
                              fune1 x
fune1 ((Symbol MINUS):b)   = do
                              x<-funt b
                              fune1 x
fune1 x                    = Return x


funt1 ((Symbol TIMES):b)   = do
                              x<-funf b
                              funt1 x
funt1 ((Symbol DIVISION):b)= do
                              x<-funf b
                              funt1 x
funt1 x                    = Return x


funf (a:b)                 = if (exp_const a) then Return b 
                                              else fX (a:b)

fX ((Id _):b)              = fuy b
fX ((Symbol LPAREN):b)     = do
                              x<- expa b
                              rec_rp x
fX (a:_)                   = Raise ("ERRORE in fX, TROVATO"++ show(a))



exp_const::Token ->Bool
exp_const (Number _)  =  True
exp_const Nil         =  True
exp_const (Bool _)    =  True
exp_const (String _)  =  True 
exp_const  _          = False


fuy ((Symbol LPAREN):b)      =  do
                                 x<-seq_exp b
                                 rec_rp x
fuy x                        = Return x


lister :: LKC -> [LKC]
lister a = [a]
lister _ = []
lister somethingelse = error("errore nella creazione lista LKC")

-- funzione nuova per gestire ,
-- devo avere lista LKC altrimenti ritorno solo l'ultimo valore
-- RPAREN sono tutte nel follow e terminali
n :: [Token] -> Exc([Token], [LKC])
n a @ (Symbol VIRGOLA : l) =
                            do
                            (before, first_trad)  <- seq_exp l      -- caso in cui dopo la virgola ho un altra espressione
                            Return(before, first_trad)

n a @ (Symbol RPAREN : _ ) =  Return (a,lister) -- sono al mio ultimo valore ( nel follow non ho altro ) quindi inzio a ritornare la 'lista' dei Token e del LKC
n a @ ( some : _ ) = error ("ci deve essere qualcosa dopo")
n _ =  error("la sinstassi non e corretta")


seq_var:: [Token]-> Exc([Token], [LKC])
seq_var a @ (Id id : l) =
                        do
                         (before, first_trad)   <- seq_var l
                         Return(before, ((lister (VAR id)) ++ first_trad))

seq_var a @ (Symbol RPAREN : l ) = Return ( l, lister )
seq_var a @ ( some : _) = error("ci deve essere qualcosa dopo, Token o ) ") -- ID
seq_var _ = error("ci deve essere un Token o ) ") -- ID o altro tipo


seq_exp:: [Token]-> Exc([Token], [LKC])
seq_exp a @ (Symbol RPAREN : _) = Return(a, lister) -- se ho la combo messa cosi allora ritorno
seq_exp a =
    do
        (next, next_trad) <- exp a
        (result, last_trad) <- n next
        Return (result, ((lister last_trad) ++ next_trad))


