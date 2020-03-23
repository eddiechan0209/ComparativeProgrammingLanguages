import Data.Char
import System.IO

-- maps labels line numbers and variables to values - uses float for line numbers for simplicity
type SymTable = [(String,Float)]

data Expr = Constant Float | Var String | Plus Expr Expr deriving (Show) 

data Stmt =
     Let String Expr |
     Print Expr deriving (Show) 

-- dummy predicate that is supposed to check if a string is a label which is a string ending with ":"
isLabel :: String -> Bool
isLabel _ = False -- there are no labels in this nano version of the tiny language

-- takes a list of tokens as strings and returns the parsed expression
parseExpr :: [String] -> Expr
parseExpr (e1:"+":e2:[]) = Plus (parseExpr [e1]) (parseExpr [e2])
parseExpr [x] = if (isAlpha (head x)) then (Var x) else (Constant (read x))

-- takes the first token which should be a keyword and a list of the remaining tokens and returns the parsed Stmt
parseStmt :: String -> [String] -> Stmt
parseStmt "let" (v:"=":expr) = Let v (parseExpr expr)
parseStmt "print" expr = Print (parseExpr expr)

-- takes a list of tokens and returns the parsed statement - the statement may include a leading label
parseLine :: [String] -> Stmt
parseLine (first:rest) =
	  if (isLabel first) then parseLine rest
	  else parseStmt first rest

-- takes a variable name and a ST and returns the value of that variable or zero if the variable is not in the ST
lookupVar :: String -> SymTable -> Float
lookupVar name [] = 0
lookupVar name ((id,v):rest) = if (id == name) then v else lookupVar name rest

-- evaluates the given Expr with the variable values found in the given ST
eval :: Expr ->SymTable -> Float
eval (Var v) env = lookupVar v env
eval (Constant v) _ = v
eval (Plus e1 e2) env = (eval e1 env) + (eval e2 env)

-- given a statement, a ST, line number, input and previous output, return an updated ST, input, output, and line number
-- this starter version ignores the input and line number
-- Stmt, SymTable, progCounter, input, output, (SymTable', input', output', progCounter)
perform:: Stmt -> SymTable -> Float -> [String] ->String -> (SymTable, [String], String, Float)
perform (Print e) env lineNum input output = (env, input, output++(show (eval e env)++"\n"), lineNum+1)
perform (Let id e) env lineNum input output = ((id,(eval e env)):env, input, output, lineNum+1)

-- given a list of Stmts, a ST, and current output, perform all of the statements in the list and return the updated output String
run :: [Stmt] -> SymTable -> String -> String
run [] _ output = output
run (curr:rest) env output = 
    let (env1, _, output1, _) = perform curr env 1 [] output in run rest env1 output1

-- given list of list of tokens, a ST, and return the list of parsed Stmts and ST storing mapping of labels to line numbers
parseTest :: [[String]] -> SymTable -> ([Stmt], SymTable)
parseTest []  st = ([], st)
-- needs completing for partial credit

main = do
     pfile <- openFile "nano1.txt" ReadMode
     contents <- hGetContents pfile
     putStr (run (map parseLine (map words (lines contents))) [] "")
     hClose pfile
