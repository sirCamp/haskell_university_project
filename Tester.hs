module Tester(
c,
d
) where

import Lexer
import Analizzatore
import Compilatore
import Interprete

c = "letrec  FACT = lambda ( X ) if  eq ( X, 0 ) then 1 else  X*FACT(  X- 1 )and G = lambda ( H L ) if  eq ( L,  nil ) then L else cons( H(car( L ) ), G ( H, cdr ( L ) )) in G ( FACT, cons(1, cons(2, cons(3, nil))) ) end $";

d = "let x=cons(\"ab\", cons(\"cd\", nil)) in if true then cons(\"01\", x) else nil end $";

tester :: String -> Int -> String
tester x y = do

            case y of
                3 -> show(run x)
                2 -> show(comp_one(x))
                1 -> show(generateLKCFromLispKit(x))
                0 -> progdoll( (lexi x))