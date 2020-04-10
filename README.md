## Command Line Interpreter

**Contents**

1. Introduction
2. Installation and operation
3. A Calculator
4. Structure

## 1. Introduction

This project is a demo in the form of a calculator 
of how one can use Elm to write a command-line interpreter.
It has a modular structure that can easily be adapted
to other, more interesting purposes.  See the section
**Structure** below.

The project consists of a `Platform.Worker` Elm program which
communicates via ports with a 17-line
Javascript program, `cli.js`.  The Javascript program
uses the Node `repl` package to provide a convenient user
interface.

### Addendum

There are two branches of interest in this project besides 
`master`.

- original
- stack

The first is the original version, as described below.
The second is a stack-based calculator, operating on the 
principles of the old HP RPN calculators.  It uses
the same architecture as does `register_version`, but
makes use of an additional `Stack` module.

## 2. Installation and operation

First do

```
$ npm install
$ npm run build
```
Then 

```
$ npm start
$ npm start
> add 2 3
5
... etc ...
> h -- provides help
```

The executable files `cli.js` and `main.js` are built
and placed in `./ex`.  
If you want to run the code without the intermediary 
of `npm`, say

```bash
$ node ex/cli.js
```


## 3.  The Calculator

The program's help screen says all that is needed:

```
Simple command line program: calculator
---------------------------------------------------------------------------------------
Operations: add, mul, etc. as described in the command summary. Saying 'add 2 3'
computes the sum and places the result in register.  If you subsequently say
'add 4', this number will be added to the contents of register M.  The
command `sub 1' will subtract 1 from register M.

This calculator has registers A, B, C, D, E, F, and M, each of which can hold a
floating point number.  Type 'r' to display register M, type 'r a' to
display the contents of register a, etc.

Command summary
---------------------------------------------------------------------------------------
> add a b         -- compute a + b, store result in register M
> mul a b         -- compute a * b, result to register M
> sub a b         -- compute a - b, result to register M
> div a b         -- compute a / b, result to register M
> exp x           -- exponential of x
> ln x            -- natural logarithm of x
> pow a x         -- a^x
> log a x         -- logarithm of x to base a
> d               -- display contents of register M
> d a             -- display contents of register A
> rcl a           -- store contents of A in M
> sto a           -- store contents of M in A
> h               -- show help
---------------------------------------------------------------------------------------
```

## 4. Structure

The project consists of five files, briefly described below.
To bend the project to other uses, one must change
the function `Main.executedCommand` and the code it references
in module `Command`.


### 4.1. Main

The heart of the program is `executeCommand`.  It is
easy to configure.  The code which carries out
 the commands is housed in module
`Commmand`.

```elm
executeCommand : Model -> String -> ArgList -> String -> ( Model, Cmd Msg )
executeCommand model cmd args input =
    case cmd of
        "add" ->
            Command.op model (+) args

        "mul" ->
            Command.op model (*) args

        ... 

        "exp" ->
            Command.f1 model (\x -> e ^ x) args

        "ln" ->
            Command.f1 model (logBase e) args

        "pow" ->
            Command.f2 model (\a b -> a ^ b) args

        ... 
```

### 4.2 Command 

This is the module that (a) implements
the commands and (b) communicates with `cli.js` via ports.


### 4.3 ArgList

Functions for extracting arguments.

### 4.4 Model

Very simple.  Just some named registers of type `Maybe Float`

### 4.5 cli.js

The Javascript program that talks to the Elm program
and to the user via  an interactive repl.

