module Compiler where

import Assembler
import DataStructures
import Data.Char (isDigit)
import Data.List (isPrefixOf)

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
    | isPrefixOf "<=" str = "<=" : lexer (drop 2 str) -- If the string starts with "<=", add it to the list of tokens and call lexer again with the rest of the string
    | isPrefixOf "==" str = "==" : lexer (drop 2 str) -- If the string starts with "==", add it to the list of tokens and call lexer again with the rest of the string
    | isPrefixOf ":=" str = ":=" : lexer (drop 2 str) -- If the string starts with ":=", add it to the list of tokens and call lexer again with the rest of the string
    | otherwise = case head str of
        ' ' -> lexer (tail str) -- If it's a space, ignore it and call itself again
        '(' -> "(" : lexer (tail str) -- If it's a "(", add it to the list of tokens and call lexer again with the rest of the string
        ')' -> ")" : lexer (tail str) -- If it's a ")", add it to the list of tokens and call lexer again with the rest of the string
        ';' -> ";" : lexer (tail str) -- If it's a ";", add it to the list of tokens and call lexer again with the rest of the string
        '=' -> "=" : lexer (tail str) -- If it's a "=", add it to the list of tokens and call lexer again with the rest of the string
        '+' -> "+" : lexer (tail str) -- If it's a "+", add it to the list of tokens and call lexer again with the rest of the string
        '-' -> "-" : lexer (tail str) -- If it's a "-", add it to the list of tokens and call lexer again with the rest of the string
        '*' -> "*" : lexer (tail str) -- If it's a "*", add it to the list of tokens and call lexer again with the rest of the string
        ':' -> ":" : "=" : lexer (tail str) -- If it's a "*", add it to the list of tokens and call lexer again with the rest of the string
        _   -> (head str : takeWhile condition (tail str)) : lexer (dropWhile condition (tail str)) -- If it's anything else, it will take the first character and then take all the characters until it finds a character that's not a letter or a digit and then call itself again with the rest of the string
  where
    condition x = notElem x " ()=;=+-*<:"
     
              
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
            (stm1, stm2) = break (== "else") (tail rest) -- It will split the list in two, before and after the "else", since before it has the statements of "then" and after it has the statements of "else"
        case head (tail stm2) of -- It will check if the first element of the statements of "else" is a "(" or not. In other words, if the "else" has multiple statements or not, if it does, they're handled by buildData, if not, buildStm can handle it. In any case, the statement of "then" are handled by buildData for ";" handling
            "(" -> If (buildBexp (tail bexp)) (buildData stm1) (buildData (tail stm2)) -- If it's a "(", it will return an If with the built bolean expression obtained, the built "then" statements (or statement) obtained and the "else" statements obtained (data)
            _ -> If (buildBexp (tail bexp)) (buildData stm1) [buildStm (tail stm2)] -- If it's not a "(", it will return an If with the built bolean expression obtained, the built "then" statements (or statement) obtained and the "else" statement obtained (it can be anything, even another if statement)
    "while" -> do -- If it finds a "while"
        let (bexp, stm) = break (== "do") list -- It will split the list in two, before and after the "do", since before it has a boolean expression and after it has the statements
        case head (tail stm) of -- It will check if the first element of the statements of "do" is a "(" or not. In other words, if the "do" has multiple statements or not, if it does, they're handled by buildData, if not, buildStm can handle it.
            "(" -> While (buildBexp (tail bexp)) (buildData (tail stm)) -- If it's a "(", it will return a While with the built bolean expression obtained and the built "do" statements obtained (data)
            _ -> While (buildBexp (tail bexp)) [buildStm (tail stm)] -- If it's not a "(", it will return a While with the built bolean expression obtained and the built "do" statement obtained (it can be anything, even another while statement)
    _ -> do -- If it's not an "if" or a "while", it's an assignment
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