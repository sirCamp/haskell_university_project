# LispKit Compiler

This a LispKit Compiler made with Haskell

This project is composed by:

+ **Lexer.hs**:  That analize and validate the LISP strings in order to find some possible error.
+ **Analizzatore.hs**: It's the part that transaltes the TOKEN that comes from the Lexer into and mid-way language called LKC
                   This languages is usefull to create the *parser tree* of LispKit in order to define the Lisp functions and Expressions
+ **Compiler.hs**: It's the responsable of the compiling of the Lisp string, this module create and produce the code tha will run on the SEDC vortual machine
+ **Interprete.hs** It's the responsable of interpretation of the code produced by the Compiler, this code is necessary to create RA, dinamyc and static environment of function and the activation stack.


## Note

**Usage**: 

- clone the repo 
- enter in the repo's folder
- type :
```bash
ghci 
```
- the type:
```haskell
:l Interprete.hs 
```
- to run som LispKit functions:
```haskell
run "let x = 5 and y = 6 in x*3 + y*2*x + x*y end $" 
```
**GLC Grammar and parsing**:

Inside the *documenti* folder there are four files, 2 in excell and 2 in word that contains the rules of the grammar and the parsing table for the generation of *Analizzatore.hs*

**Haskell version > 7.8.4**:
To work with haskell 7.10 or newer you must decomment the rows that defines the functor in the Analizzatore.hs

## Test

**Usage**: 

- clone the repo 
- enter in the repo's folder
- type :
```bash
ghci 
```
- the type:
```haskell
:l Tester.hs 
```
- to test the Lexer.hs:
```haskell
tester "let x = 5 and y = 6 in x*3 + y*2*x + x*y end $" 4
```
- to test the Analizzatore.hs:
```haskell
tester "let x = 5 and y = 6 in x*3 + y*2*x + x*y end $" 1
```
- to test the Compiler.hs:
```haskell
tester "let x = 5 and y = 6 in x*3 + y*2*x + x*y end $" 2
```
- to test the Interprete.hs:
```haskell
tester "let x = 5 and y = 6 in x*3 + y*2*x + x*y end $" 3
```
