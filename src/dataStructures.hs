module DataStructures where

import Data.List (sortBy, intercalate)

----- Assembler Data Structures -----

-- Data type for machine instructions
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
-- Data type for machine code
type Code = [Inst]

-- Data type for variables
type Variable = String              -- Variable name
-- Data type for values
data Value 
  = MyInt Integer                   -- Integer value
  | MyBool Bool                     -- Boolean value
  deriving Show
-- Data type for the machine stack
type Stack = [Value]
-- Data type for the machine state (storage)
type State = [(Variable, Value)]    -- Variable name and value

-- Auxiliary function to create an empty stack
createEmptyStack :: Stack
createEmptyStack = []

-- Auxiliary function to extract the actual value from a Value data type
showVal :: Value -> String
showVal (MyInt intVal) = show intVal
showVal (MyBool True)  = "True"
showVal (MyBool False) = "False"

-- Auxiliary function to convert the stack to a string
stack2Str :: Stack -> String
stack2Str []     = ""
stack2Str [x]    = showVal x
stack2Str (x:xs) = showVal x ++ "," ++ stack2Str xs  

-- Auxiliary function to create an empty stack
createEmptyState :: State
createEmptyState = []

-- Auxiliary function to convert the state to a string
state2Str :: State -> String
state2Str state = intercalate "," [var ++ "=" ++ showVal val | (var, val) <- sortedState]
  where
    sortedState = sortBy (\(var, _) (val, _) -> compare var val) state


----- Compiler Data Structures -----

-- Data type for arithmetic expressions
data Aexp
  = Var String                  -- Variable
  | Num Integer                 -- Numeric constant
  | AddA Aexp Aexp              -- Addition
  | SubA Aexp Aexp              -- Subtraction
  | MultA Aexp Aexp             -- Multiplication
  deriving Show

-- Data type for boolean expressions
data Bexp
  = TrueB                       -- Boolean True
  | FalseB                      -- Boolean False
  | AndB Bexp Bexp              -- And operation
  | IntEqual Aexp Aexp          -- Integer Equality comparison
  | BoolEqual Bexp Bexp         -- Boolean Equality comparison
  | LessEqual Aexp Aexp         -- Less than or equal comparison
  | NegB Bexp                   -- Negation
  deriving Show

-- Data type for statements
data Stm
  = Assign String Aexp          -- Assignment: var := Aexp
  | If Bexp [Stm] [Stm]         -- If-then-else statement: if Bexp then Stm1 else Stm2
  | While Bexp [Stm]            -- While loop: while Bexp do Stm
  deriving Show

type Program = [Stm]
