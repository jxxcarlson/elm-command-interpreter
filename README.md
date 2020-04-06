## Command Line Interereter

This project is a demo in the form of a calculator 
of how one can use Elm to write a command-line interpreter.
It has a modular structure that can easily be adapted
to other, more interesting purposes.  See the section
**Structure** below.

The project consists of an `Platform.Worker` Elm program which
communicates with the outside world via ports and a 17-line
Javascript program, `cli.js`

## Installation and operation

First do

```
$ npm install
$ npm run build
```
Then 

```
$ npm run
> add 2 3
5
... etc ...
> h -- provides helpNo I wasn't actually cause I'm preoccupied with something
```

The executable files `cli.js` and `main.js` in `./ex`.  
If you want to run the code without the intermediary 
of `npm`, say

```bash
$ node ex/cli.js
```


## Description

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

## Structure


### Main

The heart of the program is `executeCommand`.  It is
easy to configure with the actual commands defined in module
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

### Command 

This is the module that communicates with `cli.js` via ports.


### ArgList

Functions for extracting arguments.

### Model

Very simple.  Just some named registers of type `Maybe Float`

### cli.js

The Javascript program that provides an interactive repl
and which talks to the Elm program.

