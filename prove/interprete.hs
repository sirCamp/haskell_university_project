--INTERPRETE SECD COMPLETO in Haskell
module Interprete(
interprete,
fin,
Valore(..),
) where

import Compilatore
import Analizzatore

-- ##############################################
--  Area per la definzione dei datatype
-- ##############################################

{-
  Tipo che modella gli R-valori delle variabili. Si tratta dei valori da mettere nella pila S e nell'ambiente dinamico E. In particolare CLO modella le chiusure.
  V LKC - rappresenta i valori "normali", interi, booleani, stringhe...
  OGA   - record di attivazione farlocco
  CLO [Secdexpr] [[Valore]] - Chiurusa, il primo parametro è il codice della funzione, mentre il secondo contiene la copia dell'ambiente di esecuzione 
  VLISTA [Valore] - Rappresenta una lista di parametri che può trovarsi in cima alla pila
-}
data Valore = V LKC
            | OGA 
            | CLO [Secdexpr] [[Valore]]
            | VLISTA [Valore]
          deriving(Show,Eq)

-- datatype dei valori del Dump *)
{-
  TRIPLA è il datatype per i dati da tenere nel dump
  CONR viene utilizzato per gli if-then-else
-}
data Dump = CONTR  [Secdexpr] 
          | TRIPLA [Valore][[Valore]][Secdexpr] 
          | DUMMY
        deriving(Show,Eq)



-- ##############################################
--  Funzioni d'utilità
-- ##############################################

--funzione che crea l'ambiente dinamico ricorsivo necessario per il trattamento della ricorsione. Serve nel caso Rap
{-
  lazyE viene invocata con due volta la stessa lista dei valori dei binders 
-}
lazyE::[Valore]-> [Valore]->[Valore] 
lazyE [] _ = []
{-
  Se la lista non è vuota esegue lazyClo sull'elemento in testa, dopodiché continua ricorsivamente sui valori successivi
-} 
lazyE (a:b) c = ((lazyClo a c):(lazyE b c))

lazyClo:: Valore -> [Valore] -> Valore
{- 
  Caso con una chiusura.
  Viene creata una nuova chiusura con lo stesso body e come ambiente lo stesso ambienete con in cima la lista dei parametri resa ricorsiva.

  Potrebbe esserci un errore in questa funzione
-}
lazyClo (CLO a b) c = (CLO a ((lazyE c c):(tail b))) -- è necessario rimuovere [OGA] anche dall'ambiente delle chiusure
lazyClo (V x) _= (V x) 
lazyClo (VLISTA x) _= (VLISTA x)
lazyClo x _= error ("LazyClo trova valore incompatibile" ++ (show x))


--funzioni per la ricerca degli R-valori dati i loro indirizzi: usate da Ld

index::Integer ->[a]->a
index n  s= if n==0 then (head s) else (index (n-1) (tail s))  

locate::(Integer, Integer)-> [[Valore]]->Valore
locate  (a,b)  e = (index b (index a e));
      
extract_int (V (NUM x)) = x 
extract_int x = error ("trovato altro da intero" ++ (show x))

--funzioni per le liste di Valori VLISTA 

-- vhd: estrae il primo elemento dalla VLISTA ricevuta come parametro
vhd (VLISTA (a:b)) = a 
vhd (VLISTA [])  = error "vhd trovata lista vuota"
vhd _ = error "vhd non trova VLISTA"

-- vhd: ritorna la coda della VLISTA ricevuta come parametro (tutta la lista meno la testa)
vtl (VLISTA (a:b)) = VLISTA b  
vtl (VLISTA [])  = error "vtl trovata lista vuota";
vtl _ = error "vtl non trova VLISTA"

-- vatom: test che verifica se il valore ricevuto come parametro è atomico o no
vatom :: Valore -> Valore
vatom (V k)= V (BOO True)
vatom _ = V (BOO False)     
                     
-- bool2s_espressione: funzione che trasfroma un booleano in un booleano LKC       
bool2s_espressione :: Bool -> LKC            
bool2s_espressione b = if b then (BOO True) else (BOO False)

-- eqValore: test di uguaglianza per il tipo Valore, si adatta ai tipi dei parametri con cui è invocata
eqValore :: Valore -> Valore -> Bool
eqValore a@(V _) b = (eqV a b)
eqValore a@(VLISTA _) b = (eqVLISTA a b)
eqValore a  b = error ("uguaglianza tra chiusure"++ (show a) ++ (show b))

eqVLISTA :: Valore -> Valore ->Bool
eqVLISTA (VLISTA []) (VLISTA [])= True 
eqVLISTA (VLISTA(a:b)) (VLISTA (c:d)) = (eqValore a c) && (eqVLISTA (VLISTA b) (VLISTA d))
eqVLISTA _ _= False

eqV :: Valore -> Valore -> Bool
eqV (V a) (V b)= a==b
eqV _ _= False


-- ##############################################
--  Interprete
-- ##############################################
 
--FUNZIONE PRINCIPALE
{-
  s::[Valore]   - Stack
  e::[[Valore]] - Ambiente dinamico, sono i record di attivazione
  c::[Secdexpr] - Controllo, il programma in esecuzione
  d::[Dump]     - Dump
-}
interprete :: [Valore] -> [[Valore]] -> [Secdexpr] -> [Dump] -> Valore
interprete s e c d = 
  case (head c) of 
                    -- Carica il valore in (b,n) nello stack
                    Ld(b, n) -> let val = (locate (b,n) e)
                                in (interprete (val:s) e (tail c) d)
                    -- Carica una costante in cima allo stack,
                    Ldc k    -> case k of NIL -> (interprete ((VLISTA []):s) e (tail c) d)
                                          _   -> (interprete ((V k):s) e (tail c) d)
                    -- Somma i primi due valori presenti nello stack, il risultato viene messo in cima allo stack
                    Add -> let operand1 = extract_int (head s)
                               operand2 = extract_int (head (tail s))
                           in  (interprete ((V(NUM (operand1 + operand2))):(tail (tail s))) e (tail c)  d)
                    -- Sottrare i primi due valori presenti nello stack, il risultato viene messo in cima allo stack
                    Sub -> let operand1 = extract_int (head s)
                               operand2 = extract_int(head (tail s))
                           in  (interprete ((V(NUM (operand1 - operand2))):(tail (tail s))) e (tail c)  d)
                    -- Molt. i primi due valori presenti nello stack, il risultato viene messo in cima allo stack
                    Mult -> let operand1 = extract_int (head s)
                                operand2 = extract_int(head (tail s))
                            in  (interprete ((V(NUM (operand1 * operand2))):(tail (tail s))) e (tail c)  d)
                    -- Divide i primi due valori presenti nello stack, il risultato viene messo in cima allo stack
                    Div -> let operand1 = extract_int (head s)
                               operand2 = extract_int(head (tail s))
                           in  (interprete ((V(NUM (operand1 `div` operand2))):(tail (tail s))) e (tail c)  d)
                    -- Modulo dei primi due valori presenti nello stack, il risultato viene messo in cima allo stack
                    Rem -> let operand1 = extract_int (head s)
                               operand2 = extract_int(head (tail s))
                           in  (interprete ((V(NUM (operand1 `mod` operand2))):(tail (tail s))) e (tail c)  d)
                     -- Confronto dei primi due valori presenti nello stack, il risultato viene messo in cima allo stack
                    Leq -> let operand1 = extract_int (head s)
                               operand2 = extract_int(head (tail s))
                               expression = bool2s_espressione (operand1 <= operand2)
                           in  (interprete ((V (expression)):(tail (tail s))) e (tail c)  d)
                    -- Uguaglianza tra i primi due valori dello stack
                    Eq -> case s of 
                                (w1:w2:w3) -> (interprete ((V (bool2s_espressione (eqValore w1 w2))):w3) e (tail c) d)
                                _-> error "manca un argomento in Eq"
                    -- Estra dalla lista presente in cima allo stack il primo elemento e lo mette in cima
                    Car -> (interprete ((vhd(head s) ):(tail s)) e (tail c) d)
                    -- Estre la coda della lista presente in cima allo stack e la mette in cima
                    Cdr -> (interprete ((vtl(head s) ):(tail s)) e (tail c) d)
                    -- Inserisce il primo elemento dello stack in testa alla lista che si trova
                    -- come secondo elemento dello stack
                    Cons -> 
                        case head (tail s) of 
                                  (VLISTA x) -> (interprete (VLISTA ((head s):x):(tail (tail s))) e (tail c) d)
                                  x -> error ("CONS: il secondo argomento non e' una lista" ++ (show  x))
                    -- Controlla se il primo elemento dello stack è un valore atomico
                    Atom ->  (interprete ((vatom (head s)):(tail s)) e (tail c) d)
                    -- Valuta il booleano che si trova in cima allo stack, in base al valore sceglie che blocco eseguire
                    -- nel dump viene inserito in testa ((CONTR (tail c)):d ovvero il resto del programma
                    Sel sl1 sl2 -> case head s of (V (BOO True)) -> (interprete (tail s) e sl1 ((CONTR (tail c)):d))
                                                  (V (BOO False)) -> (interprete (tail s) e sl2 ((CONTR (tail c)):d))
                                                  _ -> error "non c'e' bool su s quando si esegue SEL"
                    -- Ripristina il programma alla fine dell'esecuzione di un blocco di codice andando a recuperare il
                    -- codice dal Dump
                    Join -> case (head d) of (CONTR c1) -> (interprete s e c1 (tail d))
                                             _ -> error "JOIN: il dump non contiene controllo"
                    {-
                      Costruzione della chiusura di una funzione, viene messo in cima allo stack il valore chiusura con il codice e con la copia dell'ambiente d'esecuzione, l'esecuzione continua dall'istruzione successiva.
                      sl è il codice della funzione
                    -}
                    Ldf sl -> (interprete ((CLO sl e):s) e (tail c) d)
                    -- Invoca la funzione che si trova in cima allo stack
                    -- come secondo elemento dello stack c'è la lista dei parametri attuali della funzione
                    Ap -> case (head s) of 
                            (CLO cf ef) -> case (head (tail s)) of
                                        -- la funzione viene eseguita con
                                        --   * stack vuoto
                                        --   * ambiente composto da x (parametri attuali) seguito da e1 che è 
                                        --     l'ambiente di definizione delle funzione
                                        --   * c1, ovvero la lista di [Secdexpr] che compone il corpo della funzione
                                        -- Nel dump viene salvato lo stato corrente della macchina, ovvero lo stack 
                                        -- senza la chiusura e la lista dei parametri attuali, l'ambiente corrente e 
                                        -- il resto del codice ancora da eseguire
                                        VLISTA x -> (interprete [] (x:ef) cf ((TRIPLA (tail(tail s)) e (tail c)):d))
                                        _  -> error "AP senza lista dei parametri"
                              
                            _  -> error "AP senza chiusura su s"
                    -- Recupera dal Dump lo stato della macchina prima dell'invocazione della funzione
                    -- Nello stack viene aggiunto in cima (head s) che è il valore ritornato dalla funzione
                    Rtn ->  case (head d) of (TRIPLA s1 e1 c1) -> (interprete ((head s):s1) e1 c1 (tail d))
                                             _ ->  error  "RTN: non trovata TRIPLA su dump"   
                    -- Invocazione di una funzione ricorsiva, il primo elemento dello stack deve essere una chiusura
                    -- mentre il secondo è la lista dei parametri attuali
                    Rap -> case (head s) of  
                        (CLO c1 e1) ->  case e1 of
                                     -- Il primo record di attivazione dell'ambiente della chiusura deve essere 
                                     -- un record [OGA], altrimenti è necessario sollevare un'errore
                                     ([OGA]:re) -> case (head (tail s)) of
                                                   {-
                                                      Nello stack, sotto la chiusura, deve esserci la VLISTA contenente i valori delle parti destre dei binders.
                                                      Se c'è viene eseguito il corpo della funzione con:
                                                        - stack vuoto
                                                        - ambiente dinamico con in cima la lista delle parti destre dei binders e sotto l'ambiente della chiusura. (LazyE ...) fa si che la lista dei parametri diventi circolare e viene messo al posto di OGA. 
                                                        - come controllo c'è il codice della funzione
                                                      Viene poi fatto il dump dello stato attuale
                                                   -}
                                                    (VLISTA binders_vals) -> (interprete [] ((lazyE binders_vals binders_vals):re) c1 ((TRIPLA (tail (tail s)) (tail e) (tail c)):d))
                                                    _ -> error "manca [OGA] sull'ambiente di chiusura ric"
                                     _ -> error "non trovata [OGA] nell'ambiente di chiusura ricorsiva"
                        _  -> error "RAP: non trovata chiusura su s"
                    -- Aggiunge all'ambiente un record di attivazione farlocco
                    Push ->(interprete s  ([OGA]:e)  (tail c)  d)
                    -- Programma terminato, viene ritornata la cima dello stack
                    Stop -> (head s)
                    _  -> error "operazione non riconosciuta"

-- fin: invocazione compatta dell'interprete, aggiunge [Stop] alla fine del programma x
fin x = (interprete [] [] (x ++ [Stop]) [])  

