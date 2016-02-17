module Tester(
c,
d,
e,
g,
i,
l
) where

import Lexer
import Analizzatore
import Compilatore
import Interprete

c = "letrec  FACT = lambda ( X ) if  eq ( X, 0 ) then 1 else  X*FACT(  X- 1 )and G = lambda ( H L ) if  eq ( L,  nil ) then L else cons( H(car( L ) ), G ( H, cdr ( L ) )) in G ( FACT, cons(1, cons(2, cons(3, nil))) ) end $";

d = "let x=cons(\"ab\", cons(\"cd\", nil)) in if true then cons(\"01\", x) else nil end $";

e = "let z=2 in letrec x= 2+z and y= 2*z in x*y*z end end $"

g="let f1 = lambda() letrec f2=lambda (z) if eq(z , 1) then 1 else z * f2( z - 1 ) in f2 end in let x=f1() in x(8) end end $"

i="letrec  FACT = lambda ( X ) if  eq ( X, 0 ) then 1 else  X*FACT(  X - 1 ) and G = lambda ( H L ) if  eq ( nil, L ) then L else c--ons( H(car( L ) ), G ( H, cdr ( L ) )) in G ( FACT, cons(1 ,cons(2, cons(3, nil))) ) end $"

l= "let x= 5 and y= 6 in x*3 + y * 2* x + x*y end $"

tester :: String -> Int -> String
tester x y = do

            case y of
                3 -> show(run x)
                2 -> show(comp_one(x))
                1 -> show(generateLKCFromLispKit(x))
                0 -> progdoll( (lexi x))