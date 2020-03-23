import Data.Char
import Data.List.Split
import System.IO
import System.Environment

-- maps labels line numbers and variables to values - uses float for line numbers for simplicity
type SymTable = [(String,Float)]

data Expr = Constant Float | Var String | Str String |
     LE_ Expr Expr |
     GE_ Expr Expr |
     LT_ Expr Expr |
     GT_ Expr Expr |
     EQ_ Expr Expr |
     NEQ_ Expr Expr |
     Minus Expr Expr |
     Times Expr Expr |
     Div Expr Expr |
     ExprError String |
     Plus Expr Expr deriving (Show)

data Stmt =
     Let String Expr |
     If Expr String |
     Input String |
     Error String |
     Print [Expr] deriving (Show)

-- dummy predicate that is supposed to check if a string is a label which is a string ending with ":"
isLabel :: String -> Bool
isLabel x = last x == ':'

-- takes a list of tokens as strings and returns the parsed expression
parseExpr :: [String] -> Expr
parseExpr (e1:"+":e2:[]) = Plus (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"-":e2:[]) = Minus (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"*":e2:[]) = Times (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"/":e2:[]) = Div (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"<":e2:[]) = LT_ (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:">":e2:[]) = GT_ (parseExpr[e1]) (parseExpr [e2])
parseExpr (e1:"<=":e2:[]) = LE_ (parseExpr[e1]) (parseExpr [e2])
parseExpr (e1:">=":e2:[]) = GE_ (parseExpr[e1]) (parseExpr [e2])
parseExpr (e1:"==":e2:[]) = EQ_ (parseExpr[e1]) (parseExpr [e2])
parseExpr (e1:"!=":e2:[]) = NEQ_ (parseExpr[e1]) (parseExpr[e2])
parseExpr [x] = if (isAlpha (head x)) then (Var x) else if (isQuote x) then Str (init (tail x)) else (Constant (read x))


-- takes the first token which should be a keyword and a list of the remaining tokens and returns the parsed Stmt
parseStmt :: String -> [String] -> Stmt
parseStmt "let" (v:"=":expr) = Let v (parseExpr expr)
parseStmt "print" listStrings = Print (expressionize listStrings)  -- modify so that Print is called on a list of Expressions
parseStmt "input" [val] = Input val
parseStmt "if" (e1:op:e2:"goto":label:[]) = If (parseExpr (e1:op:e2:[])) label

-- takes a variable name and a ST and returns the value of that variable or zero if the variable is not in the ST
lookupVar :: String -> SymTable -> Float
lookupVar name [] = 0
lookupVar name ((id,v):rest) = if (id == name) then v else lookupVar name rest

convert :: Bool -> Float
convert x = if x == True then 1.0 else 0.0

-- evaluates the given Expr with the variable values found in the given ST
-- returns a value: either 1.0 if true or 0.0 if false in equality cases
eval :: Expr ->SymTable -> Float
eval (Var v) env = lookupVar v env
eval (Constant v) _ = v
eval (Plus e1 e2) env = (eval e1 env) + (eval e2 env)
eval (Minus e1 e2) env = (eval e1 env) - (eval e2 env)
eval (Times e1 e2) env = (eval e1 env) * (eval e2 env)
eval (Div e1 e2) env = (eval e1 env) / (eval e2 env)
eval (LT_ e1 e2) env = convert ((eval e1 env) < (eval e2 env))
eval (GT_ e1 e2) env = convert ((eval e1 env) > (eval e2 env))
eval (LE_ e1 e2) env = convert ((eval e1 env) <= (eval e2 env))
eval (GE_ e1 e2) env = convert ((eval e1 env) >= (eval e2 env))
eval (EQ_ e1 e2) env = convert ((eval e1 env) == (eval e2 env))
eval (NEQ_ e1 e2) env = convert ((eval e1 env) /= (eval e2 env))
eval (Str _) _= -2014.23561000042 -- rare number


-- given a statement, a ST, line number, input and previous output, return an updated ST, input, output, and line number
-- this starter version ignores the input and line number
-- Stmt, SymTable, progCounter, input, output, (SymTable', input', output', progCounter)
-- perform for if: eval is compared to 1.0, look at eval for reasoning
perform :: Stmt -> SymTable -> Float -> [String] ->String -> (SymTable, [String], String, Float)
perform (Let id e) env lineNum input output = ((id,(eval e env)):env, input, output, lineNum+1)
perform (If e label) env lineNum input output = (if (eval e env) == 1.0  then (env, input, output, lookupVar label env) else 
                                                (env, input, output, lineNum+1))
perform (Print e) env lineNum input output = (env, input, (output ++ (performExpr e env) ++"\n"), lineNum+1)
-- perform (Input name) env lineNum input output = ((name, (getInput name)):env, name, output, lineNum+1)


-- given a list of Stmts, a ST, and current output, perform all of the statements in the list and return the updated output String
run :: [(Stmt,Float)] -> Float -> SymTable -> String -> String
run listTuple lineNum env output = let (env1, _, output1, lineNum) = perform (findStmt listTuple lineNum) env lineNum [] output 
									in if (round lineNum > length listTuple) then output else run listTuple lineNum env1 output1    

-- given list of list of tokens, a ST, and return the list of parsed Stmts and ST storing mapping of labels to line numbers
parseTest :: [[String]] -> SymTable -> ([Stmt], SymTable)
parseTest []  env = ([], env)
parseTest listList env = (map oldParseLine listList, parseAll (zipLineNum listList) env)
-- needs completing for partial credit

main = do
	(arg:_) <- getArgs                       
	pfile <- openFile arg ReadMode
	contents <- hGetContents pfile
	let x  = map words (lines contents)
	let st = parseAll (zipLineNum x) []
	let listStmt = map oldParseLine x
	putStr (run (zipLine listStmt) 1.0 st "")
	hClose pfile                                                          

-- HELPER FUNCTIONS --
findStmt :: [(Stmt,Float)]->Float -> Stmt
findStmt listTuple lineNum = fst (listTuple !! (round lineNum - 1))

zipLine :: [Stmt] -> [(Stmt, Float)]
zipLine x = zip x [1.0,2.0..]

parseAll :: [([String],Float)] -> SymTable -> SymTable
parseAll [] env = env
parseAll (first:rest) env = parseAll rest (updateST first env)

updateST :: ([String], Float) -> SymTable -> SymTable
updateST ((first:rest), lineNum) env =
    if (isLabel first) then (first, lineNum):env else env 

zipLineNum :: [[String]] -> [([String],Float)]
zipLineNum x = zip x [1.0,2.0 ..]

-- takes a list of tokens and returns the parsed statement - the statement may include a leading label
oldParseLine :: [String] -> Stmt
oldParseLine (first:rest) =
	  if (isLabel first) then oldParseLine rest
	  else parseStmt first rest

-- expressionize listStrings should call parseExpr
performExpr :: [Expr] -> SymTable -> String
performExpr [] _ = ""
performExpr (first:rest) env = if (eval first env == -2014.23561000042) then (init (tail (tail (tail (tail (tail (show first))))))) ++ " " ++ (performExpr rest env)
							   else (show (eval first env) ++ " " ++ (performExpr rest env))

expressionize :: [String] -> [Expr]
expressionize [] = []
expressionize listStrings = map parseExpr(map words (map deleteSpace(splitOn (",") (unwords listStrings))))

deleteSpace :: String -> String
deleteSpace [] = []
deleteSpace x = if(isASpace x) then tail x else x

isQuote :: String -> Bool
isQuote x = head x == '"'

-- takes in a word and returns whether there's a comma at the very end
isComma :: String -> Bool
isComma x = last x == ','

isASpace :: String -> Bool
isASpace x = head x == ' '

--let test = [[]]
--let stmt = Print [ConstS "done",Plus (Var "x") (Var "z"), Var "hello"]
--let st = [("x",5.0),("z",23.0),("hello",3.0)]

--let example = [("repeat"),3]