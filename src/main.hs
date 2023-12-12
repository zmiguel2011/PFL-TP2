import Data.List (sortBy, intercalate)

-- Part 1

-- Data type for machine instructions
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving (Show)
-- Data type for machine code
type Code = [Inst]

-- Data type for variables
type Variable = String
-- Data type for values
data Value 
  = MyInt Integer 
  | MyBool Bool 
  deriving (Show)
-- Data type for the machine stack
type Stack = [Value]
-- Data type for the machine state (storage)
type State = [(Variable, Value)]

-- STACK FUNCTIONS --

-- Auxiliary function for push an element to the stack
push :: Value -> Stack -> Stack
push x xs = x:xs

-- Auxiliary function to pop the first element
pop :: Stack -> Stack
pop (x:xs) = xs
pop _      = error "pop: empty stack"

-- Auxiliary function to retrieve the top element
top :: Stack -> Value
top (x:_) = x
top _     = error "top: empty stack"

-- Auxiliary function to check whether the stack is empty
isEmpty :: Stack -> Bool
isEmpty [] = True
isEmpty _  = False


-- MACHINE INSTRUCTION FUNCTIONS --

-- Auxiliary function to calculate and push the result of add, mult and sub operations onto the stack
calc :: Stack -> String -> Stack
calc stk arg    -- the pattern guard MyInt x <- top stk ensures the top of stack is in fact an Integer (MyInt)
  | arg == "+", MyInt x <- top stk = push (MyInt (x + intValue (top popStack))) finalStack
  | arg == "*", MyInt x <- top stk = push (MyInt (x * intValue (top popStack))) finalStack
  | arg == "-", MyInt x <- top stk = push (MyInt (x - intValue (top popStack))) finalStack
  | arg == "==", MyInt x <- top stk = push (MyBool (x == intValue (top popStack))) finalStack
  | arg == "==", MyBool x <- top stk = push (MyBool (x == boolValue (top popStack))) finalStack
  | arg == "<=", MyInt x <- top stk = push (MyBool (x <= intValue (top popStack))) finalStack
  | arg == "&&", MyBool x <- top stk = push (MyBool (x && boolValue (top popStack))) finalStack
  | arg == "neg", MyBool x <- top stk = push (MyBool (not x)) popStack
  | otherwise = error "Invalid operation or operands"
  where
    intValue (MyInt x)   = x
    intValue _           = error "Invalid operand type"
    boolValue (MyBool x) = x
    boolValue _          = error "Invalid operand type"
    popStack             = pop stk
    finalStack           = pop popStack

-- Auxiliary function to remove a Variable-Value pair in State
removeVar :: Variable -> [(Variable, Value)] -> [(Variable, Value)]
removeVar key = filter ((key /=) . fst)

-- Auxiliary function to update a Variable-Value pair in State
updateVar :: Variable -> Value -> [(Variable, Value)] -> [(Variable, Value)]
updateVar key val = ((key, val) :) . removeVar key

-- Auxiliary function for the fetch-x operation
fetchVar :: Variable -> Stack -> State -> (Stack, State)
fetchVar var stack state
  | Just value <- lookup var state = (push value stack, state)
  | otherwise                      = error $ "Variable not found: " ++ var

-- Auxiliary function for the store-x operation
storeVar :: Variable -> Stack -> State -> (Stack, State)
storeVar var stack state
  | isEmpty stack = error "Not enough operands for store operation or empty stack"
  | Just _ <- lookup var state = (newStack, updateVar var val state)
  | otherwise                  = (newStack, (var, val) : state)
  where
    val           = top stack
    newStack      = pop stack

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

-- Main function to interpret the code and run the instructions, returning the stack and the output values in the storage
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (inst:code, stack, state) = case inst of
  Push n -> run (code, push (MyInt n) stack, state)
  Add -> run (code, calc stack "+", state)
  Mult -> run (code, calc stack "*", state)
  Sub -> run (code, calc stack "-", state)
  Tru -> run (code, push (MyBool True) stack, state)
  Fals -> run (code, push (MyBool False) stack, state)
  Equ -> run (code, calc stack "==", state)
  Le -> run (code, calc stack "<=", state)
  And -> run (code, calc stack "&&", state)
  Neg -> run (code, calc stack "neg", state)
  Fetch var -> fetch var
  Store var -> store var
  Noop -> run (code, stack, state)
  Branch c1 c2 -> branch c1 c2
  Loop c1 c2 -> run (c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]] ++ code, stack, state)
  where
    fetch var = let (newStack, newState) = fetchVar var stack state
                in run (code, newStack, newState)
    store var = let (newStack, newState) = storeVar var stack state
                in run (code, newStack, newState)
    branch c1 c2
      | MyBool True <- top stack = run (c1 ++ code, newStack, state)
      | MyBool False <- top stack = run (c2 ++ code, newStack, state)
      | otherwise = error "Invalid value on the stack for branch"
      where
        newStack = pop stack


-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")

-- Part 2

-- Data type for arithmetic expressions
data Aexp
  = Var String                  -- Variable
  | Num Integer                 -- Numeric constant
  | AddA Aexp Aexp              -- Addition
  | SubA Aexp Aexp              -- Subtraction
  | MultA Aexp Aexp             -- Multiplication
  deriving (Show)

-- Data type for boolean expressions
data Bexp
  = TrueB                       -- Boolean True
  | FalseB                      -- Boolean False
  | Equal Aexp Aexp             -- Equality comparison
  | LessEqual Aexp Aexp         -- Less than or equal comparison
  | Not Bexp                    -- Negation
  deriving (Show)

-- Data type for statements
data Stm
  = Assign String Aexp          -- Assignment: var := Aexp
  | Seq Stm Stm                 -- Sequence of statements: Stm1 ; Stm2
  | If Bexp Stm Stm             -- If-then-else statement: if Bexp then Stm1 else Stm2
  | While Bexp Stm              -- While loop: while Bexp do Stm
  deriving (Show)

-- COMPILER FUNCTIONS --

-- Compiler for arithmetic expressions (Aexp) into machine instructions (Code)
compA :: Aexp -> Code
compA (Var var)         = [Fetch var]
compA (Num n)           = [Push n]
compA (AddA a1 a2)      = compA a2 ++ compA a1 ++ [Add]
compA (SubA a1 a2)      = compA a2 ++ compA a1 ++ [Sub]
compA (MultA a1 a2)     = compA a2 ++ compA a1 ++ [Mult]

-- Compiler for boolean expressions (Bexp) into machine instructions (Code)
compB :: Bexp -> Code
compB TrueB             = [Tru]
compB FalseB            = [Fals]
compB (Equal a1 a2)     = compA a2 ++ compA a1 ++ [Equ]
compB (LessEqual a1 a2) = compA a2 ++ compA a1 ++ [Le]
compB (Not b1)          = compB b1 ++ [Neg]

-- Compiler for statements (Stm) into machine instructions (Code)
compile :: Stm -> Code
compile (Assign var aexp)   = compA aexp ++ [Store var]
compile (Seq stm1 stm2)     = compile stm1 ++ compile stm2
compile (If bexp stm1 stm2) = compB bexp ++ [Branch (compile stm1) (compile stm2)]
compile (While bexp stm)    = [Loop (compB bexp) (compile stm)]

-- parse :: String -> [Stm]
parse = undefined -- TODO

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1 else y := 2" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")
