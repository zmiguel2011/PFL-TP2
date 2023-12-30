# Assembler and Transpiler (source-to-source compiler)

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](LICENSE)
[![Uporto FEUP](https://img.shields.io/badge/UPorto-FEUP-brown)](https://fe.up.pt)

`A low-level machine and transpiler written in Haskell.`

## Index 

- [Identification of the topic and group](#identification-of-the-topic-and-group)
- [Installation and Execution](#installation-and-execution)
  - [Installation](#installation)
  - [Execution](#execution)
- [Assembler (First Part)](#assembler-first-part)
  - [Data Types](#data-types)
    - [Variable and Value](#variable-and-value)
    - [Stack](#stack)
    - [State](#state)
  - [Stack Functions](#stack-functions)
  - [State Functions](#state-functions)
  - [Code Interpreter](#code-interpreter)
- [Compiler and Parser (Second Part)](#compiler-and-parser-second-part)
  - [Data Types](#data-types)
      - [Arithmetic Expressions](#arithmetic-expressions)
      - [Boolean Expressions](#boolean-expressions)
      - [Statements](#statements)
  - [Compiler](#compiler)
  - [Parser](#parser)
- [Conclusions](#conclusions)


## Identification of the topic and group

- **Short description:** A source-to-source compiler and assembler written in Haskell.
- **Group:** T08_G01
- **Group members:** 
    - [José Miguel Moreira Isidro](https://github.com/zmiguel2011) (<up202006485@fe.up.pt>)
    - [José António Santos Costa](https://github.com/JaySuave) (<up202004823@fe.up.pt>)
- **Contribuition:** 
    - José Miguel Moreira Isidro: 50%
    - José António Santos Costa: 50%


## Installation and Execution

### Installation

#### Requisites

To run this game you need a running Haskell environment, preferably [GHCi](https://www.haskell.org).

### Execution

1. Open `ghci`, GHC's interactive environment;
2. Load `main.hs`, located in the `src` folder;
3. Call the `testAssembler` with a list of code instructions to run them. There are a few examples in  `tests.hs`.
4. Call the `testParser` with a the code string to parse and run. There are a few examples in  `tests.hs`.
5. To run all the predefined tests, call the `main` function with no arguments.


## Assembler (First Part)

The first part of the project was a sucess, with the `run` function succeeding in all given tests.

### Data Types

#### Variable and Value

- The Variable type represents the variable names to be stored in State (storage);
- The Value data can be either an Integer value or a Boolean. This way, we can have this data represent all values for both the Stack and States. Moreover, we opted for the representation so we can easily operate and compare values without needing to convert them to other types.

```haskell
-- Data type for variables
type Variable = String
-- Data type for values
data Value 
  = MyInt Integer 
  | MyBool Bool 
  deriving (Show)
```

#### Stack

- The Stack is a list of Values.

```haskell
-- Data type for the machine stack
type Stack = [Value]
```


#### State

- The State is a list of Variable-Value pairs.

```haskell
-- Data type for the machine state (storage)
type State = [(Variable, Value)]
```

### Functions

#### Stack Functions

These **Stack** functions are very straightforward. After convertin the stack to a string, it represents the stack as an ordered list of values, separated by commas and without spaces, with the leftmost value representing the top of the stack.

```haskell
-- Auxiliary function to create an empty stack
createEmptyStack :: Stack
createEmptyStack = []

-- Auxiliary function to convert the stack to a string
stack2Str :: Stack -> String
stack2Str []     = ""
stack2Str [x]    = showVal x
stack2Str (x:xs) = showVal x ++ "," ++ stack2Str xs  
```

Below is the definition of the `showVal` function, used to convert a **Value** data to string. This function is also used the `state2Str` function.

```haskell
-- Auxiliary function to extract the actual value from a Value data type
showVal :: Value -> String
showVal (MyInt intVal) = show intVal
showVal (MyBool True)  = "True"
showVal (MyBool False) = "False"
```

In addition to the above, we have also implemented some auxiliary functions to check and update the stack, such as `push`, `pop`, `top` and `isEmpty`.


#### State Functions

The **State** functions also straightforward, however a little more complex. After convertin the state to a string, it represents the state as an list of pairs variable-value, separated by commas and without spaces, with the pairs ordered in alphabetical order of the variable name. Each variable-value pair is represented without spaces and using an ”=”.

```haskell
-- Auxiliary function to create an empty stack
createEmptyState :: State
createEmptyState = []

-- Auxiliary function to convert the state to a string
state2Str :: State -> String
state2Str state = intercalate "," [var ++ "=" ++ showVal val | (var, val) <- sortedState]
  where
    sortedState = sortBy (\(var, _) (val, _) -> compare var val) state
```

### Code Interpreter

The `run` function is the interpreter for a simple stack-based virtual machine, which executes machine instructions represented by the `Inst` data type. The machine operates on a stack (`Stack`) and maintains a state (`State`) to store variable-value pairs. The function takes a triple `(Code, Stack, State)` as its argument, where `Code` is a list of machine instructions, `Stack` is the current state of the stack, and `State` is the storage for variables.

The `run` function iteratively processes machine instructions until the code is empty. Using `case (...) of`, it pattern matches on each instruction and performs the corresponding operation. It supports operations like arithmetic operations (`Push`, `Add`, `Mult`, `Sub`), boolean operations (`Tru`, `Fals`, `Equ`, `Le`, `And`, `Neg`), variable manipulation (`Fetch`, `Store`), control flow (`Branch`, `Loop`), and a no-operation (`Noop`). The result of each operation updates the stack and state accordingly.

The function handles errors such as attempts to perform operations on an empty stack or unexpected operand types, raising run-time errors when necessary. The `run` function returns the final state of the stack, allowing users to inspect the resulting values after executing a sequence of machine instructions.

Furthermore, we improved on the previous implementation of the `calc` (from TP classes), so it handles most of the operations performed by the machine. The code for these functions is available in `assembler.hs`.

## Compiler and Parser (Second Part)

### Data Types

#### Arithmetic Expressions


#### Boolean Expressions


#### Statements


### Compiler


### Parser


## Conclusions