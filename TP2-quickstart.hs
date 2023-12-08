import Data.List (sortBy, intercalate)


-- PFL 2023/24 - Haskell practical assignment quickstart

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

type Variable = String
data Value = MyInt Integer | MyBool Bool deriving Show
type Stack = [Value]
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


-- Auxiliary function for the fetch-x operation
fetchVar :: Variable -> Stack -> State -> (Stack, State)
fetchVar var stack state
  | Just value <- lookup var state = (push value stack, state)
  | otherwise                      = error $ "Variable not found: " ++ var

-- Auxiliary function for the store-x operation
storeVar :: Variable -> Stack -> State -> (Stack, State)
storeVar var stack state
  | isEmpty stack = error "Not enough operands for store operation or empty stack"
  | otherwise     = (newStack, (var, val) : state)
  where
    val           = top stack
    newStack      = pop stack


-- Auxiliary function to create an empty stack
createEmptyStack :: Stack
createEmptyStack = []

-- stack2Str :: Stack -> String
stack2Str = undefined -- TODO, Uncomment all the other function type declarations as you implement them

createEmptyState :: State
createEmptyState = []

state2Str :: State -> String
state2Str state = intercalate "," [var ++ "=" ++ showVal val | (var, val) <- sortedState]
  where
    sortedState            = sortBy (\(var, _) (val, _) -> compare var val) state
    showVal (MyInt intVal) = show intVal
    showVal (MyBool True)  = "True"
    showVal (MyBool False) = "False"


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
  Fetch var -> let (newStack, newState) = fetchVar var stack state
             in run (code, newStack, newState)
  Store var -> let (newStack, newState) = storeVar var stack state
             in run (code, newStack, newState)
  Noop -> run (code, stack, state)
  Branch c1 c2 -> branch c1 c2
  Loop c1 c2 -> loop c1 c2
  where
    branch c1 c2 = case pop stack of
      (MyBool True):newStack -> run (c1 ++ code, newStack, state)
      (MyBool False):newStack -> run (c2 ++ code, newStack, state)
      _ -> error "Invalid value on the stack for branch"

    loop c1 c2 = run (Branch (c1 ++ [Loop c1 c2]) [Noop] : code, stack, state)

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

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
compA = undefined -- TODO

-- compB :: Bexp -> Code
compB = undefined -- TODO

-- compile :: Program -> Code
compile = undefined -- TODO

-- parse :: String -> Program
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
