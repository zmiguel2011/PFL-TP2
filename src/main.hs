import Data.List (sortBy, intercalate, isPrefixOf)
import Text.Read (read)
import Data.Char (isDigit, isSpace, chr, isLetter)

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
  | otherwise = error "Run-time error"
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
  | otherwise                      = error "Run-time error"

-- Auxiliary function for the store-x operation
storeVar :: Variable -> Stack -> State -> (Stack, State)
storeVar var stack state
  | isEmpty stack = error "Run-time error"
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
      | otherwise = error "Run-time error"
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
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"

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
  | AndB Bexp Bexp              -- And operation
  | IntEqual Aexp Aexp          -- Integer Equality comparison
  | BoolEqual Bexp Bexp         -- Boolean Equality comparison
  | LessEqual Aexp Aexp         -- Less than or equal comparison
  | NegB Bexp                   -- Negation
  deriving (Show)

-- Data type for statements
data Stm
  = Assign String Aexp          -- Assignment: var := Aexp
  | If Bexp [Stm] [Stm]         -- If-then-else statement: if Bexp then Stm1 else Stm2
  | While Bexp [Stm]            -- While loop: while Bexp do Stm
  | NoopStm 
  | Aexp Aexp 
  | Bexp Bexp
  deriving (Show)

-- COMPILER FUNCTIONS --

-- Compiler for arithmetic expressions (Aexp) into machine instructions (Code)
compA :: Aexp -> Code
compA (Var var)         = [Fetch var]
compA (Num n)           = [Push n]
compA (AddA a1 a2)      = compA a2 ++ compA a1 ++ [Add]  -- a2 comes before a1 because the stack is LIFO
compA (SubA a1 a2)      = compA a2 ++ compA a1 ++ [Sub]  -- a2 comes before a1 because the stack is LIFO
compA (MultA a1 a2)     = compA a2 ++ compA a1 ++ [Mult] -- a2 comes before a1 because the stack is LIFO

-- Compiler for boolean expressions (Bexp) into machine instructions (Code)
compB :: Bexp -> Code
compB TrueB             = [Tru]
compB FalseB            = [Fals]
compB (AndB b1 b2)      = compB b2 ++ compB b1 ++ [And]  -- b2 comes before b1 because the stack is LIFO
compB (IntEqual a1 a2)  = compA a2 ++ compA a1 ++ [Equ]  -- a2 comes before a1 because the stack is LIFO
compB (BoolEqual b1 b2) = compB b2 ++ compB b1 ++ [Equ]  -- b2 comes before b1 because the stack is LIFO
compB (LessEqual a1 a2) = compA a2 ++ compA a1 ++ [Le]   -- a2 comes before a1 because the stack is LIFO
compB (NegB b1)         = compB b1 ++ [Neg]

-- Compiler for statements (Stm) into machine instructions (Code)
compile :: [Stm] -> Code
compile [] = []
compile (stm:rest) = case stm of
  Assign var aexp -> compA aexp ++ [Store var] ++ compile rest -- It will compile the arithmetic expression and store the result in the variable
  If bexp aexp1 aexp2 -> compB bexp ++ [Branch (compile aexp1) (compile aexp2)] ++ compile rest -- It will compile the boolean expression and branch to the statements of "then" or "else" depending on the result
  While bexp aexp -> Loop (compB bexp) (compile aexp) : compile rest -- It will compile the boolean expression and the statements of "do" and then loop them
  Aexp aexp -> compA aexp ++ compile rest -- It will compile the arithmetic expression
  Bexp bexp -> compB bexp ++ compile rest -- It will compile the boolean expression

-- Auxiliary function for parsing. Receives a string and splits it into a list of tokens (strings)
lexer :: String -> [String]
lexer [] = []
lexer str
    | isPrefixOf "<=" str = "<=" : lexer (drop 2 str)
    | isPrefixOf "==" str = "==" : lexer (drop 2 str)
    | isPrefixOf ":=" str = ":=" : lexer (drop 2 str)
    | otherwise = case head str of
        ' ' -> lexer (tail str)
        '(' -> "(" : lexer (tail str)
        ')' -> ")" : lexer (tail str)
        ';' -> ";" : lexer (tail str)
        '=' -> "=" : lexer (tail str)
        '+' -> "+" : lexer (tail str)
        '-' -> "-" : lexer (tail str)
        '*' -> "*" : lexer (tail str)
        _   -> (head str : takeWhile condition (tail str)) : lexer (dropWhile condition (tail str))
  where
    condition x = notElem x " ()=;+-*"
              
-- Receives a list of tokens and returns the built data program (as a list of statements)
buildData :: [String] -> [Stm]
buildData [] = []
buildData list =
  case findFirstNotNested [";"] list of -- It will look for the first ";" that's not nested
    Just index -> -- If it finds it
      let (stm, rest) = splitAt index list -- It will split the list in two, stm has the statement and rest has the rest of the tokens
      in if head stm == "(" -- If it's actually multiple nested statements instead of just one
           then buildData (tail (init stm))
           else case rest of
                  [_] -> [buildStm stm] -- If it's the last statement, it will return a list with the built statement obtained
                  _ -> buildStm stm : buildData (tail rest) -- If it's not the last statement, it will return a list with the built statement obtained and the rest of the statements obtained
    Nothing -> buildData (tail (init list)) -- If it doesn't find it, then any statement left is between parentheses, so it will remove the first and last element of the list (the parentheses) and call itself again (this is done last since everything between parentheses has higher priority than what is outside of it).

-- Builds a statement from a list of tokens that were already separated by buildData
buildStm :: [String] -> Stm
buildStm list = 
    case head list of
        "if" -> do -- If it finds an "if"
            let (bexp, rest) = break (== "then") list -- It will split the list in two, before and after the "then", since before it has a boolean expression and after it has the statements
            case findFirstNotNested ["else"] (tail rest) of -- It will look for the first "else" that's not nested
                Just index -> do -- If it finds it
                    let (stm1, stm2) = splitAt index (tail rest) -- It will split the list (rest) in two, stm1 has the statements that belong to the "then" and stm2 has the statements after the "else"
                    case head (tail stm2) of -- It will check if the first element of the statements of "else" is a "(" or not. In other words, if the "else" has multiple statements or not, if it does, they're handled by buildData, if not, buildStm can handle it. In any case, the statement of "then" are handled by buildData for ";" handling
                        "(" -> If (buildBexp (tail bexp)) (buildData stm1) (buildData (tail stm2)) -- If it's a "(", it will return an If with the built bolean expression obtained, the built "then" statements (or statement) obtained and the "else" statements obtained (data)
                        _ -> If (buildBexp (tail bexp)) (buildData stm1) [buildStm (tail stm2)] -- If it's not a "(", it will return an If with the built bolean expression obtained, the built "then" statements (or statement) obtained and the "else" statement obtained (it can be anything, even another if statement)
        "while" -> do -- If it finds a "while"
            let (bexp, stm) = break (== "do") list -- It will split the list in two, before and after the "do", since before it has a boolean expression and after it has the statements
            case head (tail stm) of -- It will check if the first element of the statements of "do" is a "(" or not. In other words, if the "do" has multiple statements or not, if it does, they're handled by buildData, if not, buildStm can handle it.
                "(" -> While (buildBexp (tail bexp)) (buildData (tail stm)) -- If it's a "(", it will return a While with the built bolean expression obtained and the built "do" statements obtained (data)
                _ -> While (buildBexp (tail bexp)) [buildStm (tail stm)] -- If it's not a "(", it will return a While with the built bolean expression obtained and the built "do" statement obtained (it can be anything, even another while statement)
        _ -> do -- If it's not an "if" or a "while"
            let (var, aexp) = break (== ":=") list -- It will split the list in two, before and after the ":=", since before it has a variable and after it has an arithmetic expression
            Assign (head var) (buildAexp (tail aexp)) -- It will return an Assign with the variable and the built arithmetic expression obtained

-- Auxiliary function to find the first token that's not nested in a list of tokens
findFirstNotNested :: [String] -> [String] -> Maybe Int
findFirstNotNested targets = find 0 0 -- It will call the find function with depth 0, index 0 and the list of tokens
  where
    find _ _ [] = Nothing       -- If it reaches the end of the list or given an empty list, it will return Nothing
    find depth index (x:rest)
      | x == "(" || x == "then" = find (depth + 1) (index + 1) rest -- If it finds a "(" or a "then" it will increase the depth and the index
      | x == ")" || (x == "else" && depth /= 0) = find (depth - 1) (index + 1) rest -- If it finds a ")" or an "else: it will decrease the depth and the index
      | depth == 0 && x `elem` targets = Just index   -- If it's not nested and it finds what it's looking for, it will return the index
      | otherwise = find depth (index + 1) rest       -- If it's not what it's looking for, it will increase the index and keep looking

-- Builds an arithmetic expression from a list of tokens
buildAexp :: [String] -> Aexp
buildAexp [expr] -- If it's a single token
  | all isDigit expr = Num (read expr) -- If it's a number, it will return a Num
  | otherwise = Var expr -- If it's not a number, it will return a Var
buildAexp list -- If it's multiple tokens
  | Just reversedIndex <- findFirstNotNested ["+","-"] (reverse list) = do -- It will look for the last "+" or "-" that's not nested
    let index = length list - reversedIndex - 1 -- It will calculate the real index of the last "+" or "-" that's not nested, since it's reversed
    let (before, after) = splitAt index list -- It will split the list in two, before and after the last "+" or "-" that's not nested
    if list!!index == "+" -- If it's a "+", it will return an AddA with the built arithmetic expressions of both lists obtained (before and after the "+")
      then AddA (buildAexp before) (buildAexp (tail after)) -- It will return an AddA with the built arithmetic expressions of both lists obtained (before and after the "+")  
      else SubA (buildAexp before) (buildAexp (tail after)) -- If it's a "-", it will return a SubA with the built arithmetic expressions of both lists obtained (before and after the "-")
  | Just reversedIndex <- findFirstNotNested ["*"] (reverse list) = do -- It will look for the last "*" that's not nested
    let index = length list - reversedIndex - 1 -- It will calculate the real index of the last "*" that's not nested, since it's reversed
    let (before, after) = splitAt index list -- It will split the list in two, before and after the last "*" that's not nested
    MultA (buildAexp before) (buildAexp (tail after)) -- It will return a MultA with the built arithmetic expressions of both lists obtained (before and after the "*")
  | otherwise = buildAexp (tail (init list)) -- If it doesn't find any of the above, then any expression left is between parentheses, so it will remove the first and last element of the list (the parentheses) and call itself again (this is done last since everything between parentheses has higher priority than what is outside of it).


-- Builds a boolean expression from a list of tokens
buildBexp :: [String] -> Bexp
buildBexp [expr]
  | expr == "True" = TrueB -- If it's a "True", it will return a TrueB
  | expr == "False" = FalseB -- If it's a "False", it will return a FalseB
  | otherwise = error "Run-time error" -- If it's neither, it will return an error
buildBexp list
  | Just reversedIndex <- findFirstNotNested ["and"] (reverse list) = do -- It will look for the last "and" that's not nested
    let index = length list - reversedIndex - 1 -- It will calculate the real index of the last "and" that's not nested, since it's reversed
    let (before, after) = splitAt index list -- It will split the list in two, before and after the last "and" that's not nested
    AndB (buildBexp before) (buildBexp (tail after)) -- It will return an AndB with the built boolean expressions of both lists obtained (before and after the "and")
  | Just reversedIndex <- findFirstNotNested ["="] (reverse list) = do -- It will look for the last "=" that's not nested
    let index = length list - reversedIndex - 1 -- It will calculate the real index of the last "=" that's not nested, since it's reversed
    let (before, after) = splitAt index list -- It will split the list in two, before and after the last "=" that's not nested
    BoolEqual (buildBexp before) (buildBexp (tail after)) -- It will return a BoolEqual with the built boolean expressions of both lists obtained (before and after the "=")
  | Just reversedIndex <- findFirstNotNested ["not"] (reverse list) = do -- It will look for the last "not" that's not nested
    let index = length list - reversedIndex - 1 -- It will calculate the real index of the last "not" that's not nested, since it's reversed
    let after = drop index list -- It will split the list in two, before and after the last "not" that's not nested
    NegB (buildBexp (tail after)) -- It will return a NegB with the built boolean expression of the list obtained (after the "not")
  | Just reversedIndex <- findFirstNotNested ["=="] (reverse list) = do -- It will look for the last "==" that's not nested
    let index = length list - reversedIndex - 1 -- It will calculate the real index of the last "==" that's not nested, since it's reversed
    let (before, after) = splitAt index list -- It will split the list in two, before and after the last "==" that's not nested
    IntEqual (buildAexp before) (buildAexp (tail after)) -- It will return an IntEqual with the built arithmetic expressions of both lists obtained (before and after the "==")
  | Just reversedIndex <- findFirstNotNested ["<="] (reverse list) = do -- It will look for the last "<=" that's not nested
    let index = length list - reversedIndex - 1 -- It will calculate the real index of the last "<=" that's not nested, since it's reversed
    let (before, after) = splitAt index list -- It will split the list in two, before and after the last "<=" that's not nested
    LessEqual (buildAexp before) (buildAexp (tail after)) -- It will return a LessEqual with the built arithmetic expressions of both lists obtained (before and after the "<=")
  | otherwise = buildBexp (tail (init list)) -- If it doesn't find any of the above, then any expression left is between parentheses, so it will remove the first and last element of the list (the parentheses) and call itself again (this is done last since everything between parentheses has higher priority than what is outside of it).


-- Receives a string (the program code written in the language) and returns the program
parse :: String -> [Stm]
parse = buildData . lexer -- It will call lexer to split the string into tokens and then buildData to build the program

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)


-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "x := 0 - 2;" == ("","x=-2")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")
