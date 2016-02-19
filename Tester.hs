module Tester(
a,
b,
c,
d,
f,
e,
g,
h,
i,
l,
m,
n
) where

import Lexer
import Analizzatore
import Compilatore
import Interprete


a = "let x=5 and y= 6 in x + y * 2 end$"

b = "let x=5 and y=6 in let f = lambda(x) x+5 in f(3) end   end$"

-- distribuisce FACT su una lista di interi *)
c = "letrec  FACT = lambda ( X ) if  eq ( X, 0 ) then 1 else  X*FACT(  X- 1 )and G = lambda ( H L ) if  eq ( L,  nil ) then L else cons( H(car( L ) ), G ( H, cdr ( L ) )) in G ( FACT, cons(1, cons(2, cons(3, nil))) ) end $";

d = "let x=cons(\"ab\", cons(\"cd\", nil)) in if true then cons(\"01\", x) else nil end $";

e = "let z=2 in letrec x= 2+z and y= 2*z in x*y*z end end $"

--(*considera liste di liste Z e produce una lista semplice che contiene tanti interi quante sono le liste contenute in Z e l'intero
--corrispondente ad una lista contenuta in Z è la somma dei fattoriali dei suoi elementi: f2=fattoriale, f1=calcola somma dei fattori--ali degli elementi di una
--lista di interi e f0 distribuisce f1 sulle liste contenute in Z *)

f="letrec f0 = lambda ( x ) letrec f1 = lambda(y) letrec f2=lambda (z) if eq(z , 1) then 1 else z * f2( z - 1 ) in if eq( y , nil ) then 0 else f2 ( car ( y ) ) + f1 ( cdr (y)) end  in if eq(x , nil) then nil else cons (f1 ( car ( x )),f0 ( cdr ( x ) ) ) end in f0( cons (cons (3 , cons (3 , nil)), cons( cons (3 , nil), nil))) end $"

h = "let f1 = lambda () letrec f2 = lambda (z) if eq(z,1) then 1 else z*f2(z-1) in f2 end in f1 end $"

g="let f1 = lambda() letrec f2=lambda (z) if eq(z , 1) then 1 else z * f2( z - 1 ) in f2 end in let x=f1() in x(8) end end $"

i="letrec  FACT = lambda ( X ) if  eq ( X, 0 ) then 1 else  X*FACT(  X - 1 ) and G = lambda ( H L ) if  eq ( nil, L ) then L else c--ons( H(car( L ) ), G ( H, cdr ( L ) )) in G ( FACT, cons(1 ,cons(2, cons(3, nil))) ) end $"

l= "let x= 5 and y= 6 in x*3 + y * 2* x + x*y end $"

m = "let x=5 and y= 6 in % + y * 2 end$" --errore lexer

n = "let x=5 and y= 6 in  + y * 2 end$" --errore analizzatore

tester :: String -> Int -> String
tester x y = do

            case y of
                4 -> show(lexi x)
                3 -> show(run x)
                2 -> show(comp_one(x))
                1 -> show(generateLKCFromLispKit(x))
                0 -> progdoll( (lexi x))