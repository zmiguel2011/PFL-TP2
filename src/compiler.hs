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
compile :: Program  -> Code
compile [] = []
compile (stm:rest) = case stm of
  Assign var aexp -> compA aexp ++ [Store var] ++ compile rest -- It will compile the arithmetic expression and store the result in the variable
  If bexp aexp1 aexp2 -> compB bexp ++ [Branch (compile aexp1) (compile aexp2)] ++ compile rest -- It will compile the boolean expression and branch to the statements of "then" or "else" depending on the result
  While bexp aexp -> Loop (compB bexp) (compile aexp) : compile rest -- It will compile the boolean expression and the statements of "do" and then loop them

-- Auxiliary function for parsing. Receives a string and splits it into a list of tokens (strings)
lexer :: String -> [String]
lexer [] = []
lexer str
    | "<=" `isPrefixOf` str = "<=" : lexer (drop 2 str) 
    | "==" `isPrefixOf` str = "==" : lexer (drop 2 str) 
    | ":=" `isPrefixOf` str = ":=" : lexer (drop 2 str) 
    | otherwise = case head str of
        ' ' -> lexer (tail str) -- ignore spaces
        '(' -> "(" : lexer (tail str) 
        ')' -> ")" : lexer (tail str) 
        ';' -> ";" : lexer (tail str) 
        '=' -> "=" : lexer (tail str) 
        '+' -> "+" : lexer (tail str) 
        '-' -> "-" : lexer (tail str)
        '*' -> "*" : lexer (tail str) 
        ':' -> ":" : "=" : lexer (tail str) 
        _   -> (head str : takeWhile condition (tail str)) : lexer (dropWhile condition (tail str)) -- If it's anything else, it will take the first character and then take all the characters until it finds a character that's not a letter or a digit and then call itself again with the rest of the string
  where
    condition x = x `notElem` " ()=;=+-*<:"


-- Receives a list of tokens and returns the built data program (as a list of statements)
buildData :: [String] -> Program 
buildData [] = []
buildData list 
  | Just index <- findFirstNotNested [";"] list, let (stm, rest) = splitAt index list, head stm == "(" = buildData (tail (init stm)) -- Split the list in two, before and after the ";", if it's actually multiple nested statements instead of just one
  | Just index <- findFirstNotNested [";"] list, let (stm, rest) = splitAt index list, [_] <- rest = [buildStm stm] -- Split the list in two, before and after the ";", if it's at the last statement 
  | Just index <- findFirstNotNested [";"] list, let (stm, rest) = splitAt index list = buildStm stm : buildData (tail rest) -- Split the list in two, before and after the ";", if it's at any other statement
  | otherwise = buildData (tail (init list)) 

-- Builds a statement from a list of tokens that were already separated by buildData
buildStm :: [String] -> Stm
buildStm list
  | head list == "if" = buildIfStm list
  | head list == "while" = buildWhileStm list
  | otherwise = buildAssignStm list
  where
    buildIfStm list 
      | head (tail elseStatements) == "(" = If (buildBexp (tail bexp)) (buildData thenStatements) (buildData (tail elseStatements))
      | otherwise = If (buildBexp (tail bexp)) (buildData thenStatements) [buildStm (tail elseStatements)]
      where
        (bexp, rest) = break (== "then") list
        (thenStatements, elseStatements) = break (== "else") (tail rest)
    buildWhileStm list 
      | head (tail doStatements) == "(" = While (buildBexp (tail bexp)) (buildData (tail doStatements))
      | otherwise = While (buildBexp (tail bexp)) [buildStm (tail doStatements)]
      where
        (bexp, doStatements) = break (== "do") list
    buildAssignStm list = Assign (head var) (buildAexp (tail aexp)) 
      where
        (var, aexp) = break (== ":=") list


-- Auxiliary function to find the first token that's not nested in a list of tokens
findFirstNotNested :: [String] -> [String] -> Maybe Int
findFirstNotNested targets = find 0 0 
  where
    find _ _ [] = Nothing      
    find depth index (x:rest)
      | x == "(" || x == "then" = find (depth + 1) (index + 1) rest
      | x == ")" || (x == "else" && depth /= 0) = find (depth - 1) (index + 1) rest 
      | depth == 0 && x `elem` targets = Just index
      | otherwise = find depth (index + 1) rest    

-- Builds an arithmetic expression from a list of tokens
buildAexp :: [String] -> Aexp
buildAexp [expr] -- If it's a single token
  | all isDigit expr = Num (read expr) -- If it's a number, it will return a Num
  | otherwise = Var expr -- If it's not a number, it will return a Var
buildAexp list -- If it's multiple tokens
  | Just reversedIndex <- findFirstNotNested ["+","-"] (reverse list) = do 
    let index = length list - reversedIndex - 1 
    let (before, after) = splitAt index list 
    if list!!index == "+" 
      then AddA (buildAexp before) (buildAexp (tail after)) 
      else SubA (buildAexp before) (buildAexp (tail after)) 
  | Just reversedIndex <- findFirstNotNested ["*"] (reverse list) = do 
    let index = length list - reversedIndex - 1 
    let (before, after) = splitAt index list
    MultA (buildAexp before) (buildAexp (tail after)) 
  | otherwise = buildAexp (tail (init list)) 


-- Builds a boolean expression from a list of tokens
buildBexp :: [String] -> Bexp
buildBexp [expr]
  | expr == "True" = TrueB 
  | expr == "False" = FalseB 
  | otherwise = error "Run-time error" 
buildBexp list
  | Just reversedIndex <- findFirstNotNested ["and"] (reverse list) = do 
    let index = length list - reversedIndex - 1 
    let (before, after) = splitAt index list 
    AndB (buildBexp before) (buildBexp (tail after)) 
  | Just reversedIndex <- findFirstNotNested ["="] (reverse list) = do 
    let index = length list - reversedIndex - 1 
    let (before, after) = splitAt index list 
    BoolEqual (buildBexp before) (buildBexp (tail after)) 
  | Just reversedIndex <- findFirstNotNested ["not"] (reverse list) = do 
    let index = length list - reversedIndex - 1 
    let after = drop index list
    NegB (buildBexp (tail after))
  | Just reversedIndex <- findFirstNotNested ["=="] (reverse list) = do
    let index = length list - reversedIndex - 1
    let (before, after) = splitAt index list
    IntEqual (buildAexp before) (buildAexp (tail after)) 
  | Just reversedIndex <- findFirstNotNested ["<="] (reverse list) = do
    let index = length list - reversedIndex - 1
    let (before, after) = splitAt index list 
    LessEqual (buildAexp before) (buildAexp (tail after)) 
  | otherwise = buildBexp (tail (init list))  


-- Receives a string (the program code written in the language) and returns the program
parse :: String -> Program 
parse = buildData . lexer -- It will call lexer to split the string into tokens and then buildData to build the program
