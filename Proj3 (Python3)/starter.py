#! /usr/bin/env python3
import fileinput
import sys

# used to store a parsed TL expressions which are
# constant numbers, constant strings, variable names, and binary expressions
class Expr :
    def __init__(self,op1,operator,op2=None):
        self.op1 = op1
        self.operator = operator
        self.op2 = op2

    def __str__(self):
        if self.op2 == None:
            return self.operator + " " + self.op1
        else:
            return self.op1 + " " + self.operator + " " +  self.op2

    # evaluate this expression given the environment of the symTable
    def eval(self, symTable):
        oprand1 = self.op1
        oprand2 = self.op2

        # if the first operand is a number, convert the string to num,
        # set it to oprand1
        # if the first operand is not a number -> variable
        # get the value of the variable, set it to oprand1 
        if(is_number(oprand1)):
            oprand1 = float(oprand1)
        elif(self.operator != "str"):
            oprand1 = symTable[oprand1]
        # do the same for oprand2
        if(oprand2 != None):
            if(is_number(oprand2)):
                oprand2 = float(oprand2)
            else:
                oprand2 = symTable[oprand2]

        if self.operator == "var":
            return oprand1
        elif self.operator == "str":
            return oprand1           # change to return oprand1
        elif self.operator == "const":
            return oprand1
        elif self.operator == "+":
            return oprand1 + oprand2
        elif self.operator == "-":
            return oprand1 - oprand2
        elif self.operator == "*":
            return oprand1 * oprand2
        elif self.operator == "/":
            return oprand1 / oprand2
        elif self.operator == "<":
            return oprand1 < oprand2
        elif self.operator == ">":
            return oprand1 > oprand2
        elif self.operator == "<=":
            return oprand1 <= oprand2
        elif self.operator == ">=":
            return oprand1 >= oprand2
        elif self.operator == "==":
            return oprand1 < oprand2
        elif self.operator == "!=":
            return oprand1 != oprand2
        

# used to store a parsed TL statement
class Stmt :
    def __init__(self,keyword,exprs):
        self.keyword = keyword
        self.exprs = exprs

    def __str__(self):
        others = ""
        for exp in self.exprs:
            others = others + " " + str(exp)
        return self.keyword + others

    # perform/execute this statement given the environment of the symTable
    def perform(self, symTable, lineNum):
        #print ("Doing: " + str(self))
        # performing "let"
        # objective is to update the symTable, binding the variable directly following
        # "let" with the evaluation of the expr following "="
        # two cases of "let" statement: (1) 3 tokens or (2) 5 tokens
        # ie (1) [x = 5] or (2)[x = 5 + 1]
        # two cases in (1): [x = 5] or [x = y], the last value is a const or a var
        if(self.keyword == "let"):
            length = len(self.exprs)
            if(length == 3):
                if(is_number(self.exprs[2])):
                    expr = Expr(self.exprs[2], "const")
                elif(self.exprs[2][0] == "\"" or self.exprs[2][0] == "\'"):
                    noQuotes = self.exprs[2][1:-1]
                    expr = Expr(noQuotes, "str")    # what about let x = "hello"
                else:
                    expr = Expr(self.exprs[2], "var")         
            else:
                expr = Expr(self.exprs[2], self.exprs[3], self.exprs[4])
            updateSym(self.exprs[0], expr.eval(symTable))

        # performing "if"
        # if the expression evaluates to true, return the linenumber where the label is
        # otherwise, return the incrimented linenumber
        elif(self.keyword == "if"):

            expr = Expr(self.exprs[0], self.exprs[1], self.exprs[2])     # should be 0, 1, 2
            if(expr.eval(symTable)):
                return symTable[self.exprs[4]]

        # performing "print"
        # print one or multiple expressions, which can be strings, numbers, variables,
        # or expressions with mathematical operations
        elif(self.keyword == "print"):
            lExprs = " ".join(self.exprs).split(", ")
            i = 0 # counter
            numExprs = len(lExprs)
            while(i < numExprs):
                # expression is a constant or a variable if it's length is 1
                if(len(lExprs[i]) == 1):
                    if(is_number(lExprs[i])):
                        expr = Expr(lExprs[i], "const")
                    else:
                        expr = Expr(lExprs[i], "var")
                # expression is a string if it begins with a quote
                elif(lExprs[i][0] == "\"" or "\'"):
                    expr = Expr(lExprs[i][1:-1], "str")     # delete the quotes? what if "" : still works
                # expression is a mathematical operation if not a var, num, or string
                else:
                    expr = Expr(lExprs[i][0], lExprs[i][2], lExprs[i][4])
                print(expr.eval(symTable))
                i = i + 1

        # performing "input"
        # update the symTable with the variable following "input" binded with the value
        # given by user input   
        elif(self.keyword == "input"):
            num = float(input())
            updateSym(self.exprs[0], num)

        return lineNum + 1


def updateSym(strng, val):
    symTable[strng] = val

def is_number(s):
    try:
        float(s)
        return True
    except ValueError:
        return False

def parseStmt(tokenizedLine, lineNum):
    if tokenizedLine[0][0] == "\t":
        tokenizedLine[0].strip("\t")
    if tokenizedLine[0][-1] == ":":
        label = tokenizedLine[0][0:-1]
        updateSym(label, lineNum)
        keyword = tokenizedLine[1]
        expr = tokenizedLine[2:]
        sList.append(Stmt(keyword, expr))
    else:    
        keyword = tokenizedLine[0]
        expr = tokenizedLine[1:]
        sList.append(Stmt(keyword, expr))

def run(lineNum):
    if (lineNum <= len(sList)):
        run ((sList[lineNum-1]).perform(symTable,lineNum))

# empty list of STMT objects
sList = []
# symTable stores mapping of labels to lineNum, and variables to evaluated values
symTable = {}
# lineNum keeps track of program line number. needed to update symTable with label
lineNum = 1

inputFile = open(sys.argv[1])
for line in inputFile:
    tokenizedLine = line.split()
    parseStmt(tokenizedLine, lineNum)
    lineNum = lineNum + 1

run(1)