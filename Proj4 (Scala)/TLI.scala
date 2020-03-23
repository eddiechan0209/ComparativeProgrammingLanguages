import scala.collection.mutable.Map
import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import java.io.IOException

abstract class Expr
case class Var(name: String) extends Expr
case class Str(name: String) extends Expr
case class Constant(num: Double) extends Expr
case class BinOp(operator: String, left: Expr, right: Expr) extends Expr

abstract class Stmt
case class Let(variable: String, expr: Expr) extends Stmt
case class If(expr: Expr, label: String) extends Stmt
case class Input(variable: String) extends Stmt
case class Print(exprList: List[Expr]) extends Stmt
case class Error(str: String) extends Stmt

object TLI {
    def eval(expr: Expr, symTab: Map[String, Double], lineNum: Double): Double = expr match {
        case BinOp("+",e1,e2) => eval(e1,symTab, lineNum) + eval(e2,symTab, lineNum)
        case BinOp("-",e1,e2) => eval(e1,symTab, lineNum) - eval(e2, symTab, lineNum)
        case BinOp("*",e1,e2) => eval(e1,symTab, lineNum) * eval(e2, symTab, lineNum) 
        case BinOp("/",e1,e2) => eval(e1,symTab, lineNum) / eval(e2, symTab, lineNum)
        case BinOp("<",e1,e2) => boolToDouble(eval(e1,symTab, lineNum) < eval(e2, symTab, lineNum))
        case BinOp(">",e1,e2) => boolToDouble(eval(e1,symTab, lineNum) > eval(e2, symTab, lineNum))
        case BinOp("<=",e1,e2) => boolToDouble(eval(e1,symTab, lineNum) <= eval(e2, symTab, lineNum))
        case BinOp(">=",e1,e2) => boolToDouble(eval(e1,symTab, lineNum) >= eval(e2, symTab, lineNum))
        case BinOp("==",e1,e2) => boolToDouble(eval(e1,symTab, lineNum) == eval(e2, symTab, lineNum))
        case BinOp("!=",e1,e2) => boolToDouble(eval(e1,symTab, lineNum) != eval(e2, symTab, lineNum))
        case Var(name) => symTab(name)
        case Constant(num) => num
		case _ => {
                println("Syntax error on line " + lineNum.toInt.toString + ".")
                System.exit(1)
                return 69
        }// should really throw an error
    }

    def boolToDouble(bool: Boolean): Double = {
        if(bool == true)
            return 1.0
        else
            return 0.0
    }

    def updateSym(label: String, lineNum: Double, symTab: Map[String, Double] ): Unit = {
    	symTab += label -> lineNum
    }

    def isAllDigits(x: String) = x forall Character.isDigit 		// function returns true if the string is a number		

    def getExpr(operator: String, oprand1: String, oprand2: String): Expr = {
        var leftDouble: Double = 0
        var rightDouble: Double = 0
        if(isAllDigits(oprand1) && isAllDigits(oprand2)){           // if oprand1 and oprand 2 are both numbers, 
            leftDouble = oprand1.toDouble
            val left = Constant(leftDouble)                         // left is an Expr of type Constant                   
            rightDouble = oprand2.toDouble
            val right = Constant(rightDouble)                       // right is an Expr of type Constant
            val newExpr = BinOp(operator, left, right)              // newExpr is an Expression of type BinOp(operator, left, right)
        //    val stmt = If(newExpr, label)
            return newExpr
        } 
        else if(isAllDigits(oprand1) && !isAllDigits(oprand2)){     // if oprand1 is a number and oprand2 is a variable
            leftDouble = oprand1.toDouble
            val left = Constant(leftDouble)                         // left is an Expr of type Constant
            val right = Var(oprand2)                                // right is an Expr of type Var
            val newExpr = BinOp(operator, left, right)              // newExpr is an Expression of type BinOp(operator, left, right)
        //    val stmt = If(newExpr, label)
            return newExpr
        }
        else if(!isAllDigits(oprand1) && isAllDigits(oprand2)){     // if oprand1 is a variable and oprand2 is a number
            val left = Var(oprand1)                                 // left is an Expr of type Var
            rightDouble = oprand2.toDouble
            val right = Constant(rightDouble)                       // right is an Expr of type Constant
            val newExpr = BinOp(operator, left, right)              // newExpr is an Expression of type BinOp(operator, left, right)
        //    val stmt = If(newExpr, label)
            return newExpr
                //  val right = Constant(op2)
            }
        else{                                                       // if oprand1 and oprand2 are variables
            val left = Var(oprand1)
            val right = Var(oprand2)
            val newExpr = BinOp(operator, left, right)
        //    val stmt = If(newExpr, label)
            return newExpr
        }
    }


    def getStmt(keyword: String, expr: Array[String]): Stmt = {
    	if(keyword == "let"){
            if(expr.length == 3){			            // if expression is of type let x = 1: "x = 1"
                var thirdElem = expr(2)
    			if(isAllDigits(thirdElem)){		        // if the operand is a number, the expr is of Constant type
    				val number = thirdElem.toDouble		// number stores the numeric representation of the value
    				val newExpr = Constant(number)		// newExpr is the expression of type Constant
                    val stmt = Let(expr(0), newExpr)    // stmt is a Let object of type Let(varName, newExpr)
                    return stmt
    			}
    			else{						            // else the operand is a variable, and the expr is of Var type
    				val newExpr = Var(thirdElem)
                    val stmt = Let(expr(0), newExpr)
                    return stmt
    			}
                
    		}
    		else{                                                    // if the expression is of type x = x + 1
    			val operator = expr(3)				                 
    			val oprand1 = expr(2)
    			val oprand2 = expr(4)
    			val newExpr = getExpr(operator, oprand1, oprand2)    // getExpr returns the Expr object
                val stmt = Let(expr(0), newExpr)                     // stmt is a Let object of type Let(varName, newExpr)
                return stmt
    		}
    		
    	}

    	else if(keyword == "input"){
    		val variable = expr.mkString(" ")                         // var stores the expression array in string format
    		val stmt = Input(variable)
            return stmt
    	}
    	else if(keyword == "if"){
    		val label = expr(expr.length - 1)                         // get the label
    		val oprand1 = expr(0)
    		val oprand2 = expr(2)
    		val operator = expr(1)
    		val newExpr = getExpr(operator, oprand1, oprand2)         // getExpr returns the Expr object
            val stmt = If(newExpr, label)                             // stmt is an object of type If(newExpr, label)
            return stmt
    	}
    	else if(keyword == "print"){
    		var lExprs = ListBuffer[String]()
    		var line = expr.mkString(" ")
    		var lineSplit = line.split(", ")
    		for(elem <- lineSplit){
    			lExprs += elem.trim()
    		}                                                 // at this point, lExprs looks like ListBuffer(x + 5, "done") 
    		var newList = ListBuffer[Expr]()                  // where each element is a string
            var toExpr = Array("temp")
            for(elem <- lExprs){
                if(elem(0).toString == "\"" || elem(0).toString == "\'"){
                    toExpr = Array(elem)                // if elem = "hello", toExpr = Array(hello).
                }                
                else{
                    toExpr = elem.split(" +")
                }
                if(toExpr.length == 1){
                    if(isAllDigits(toExpr(0))){
                        val expr = Constant(toExpr(0).toDouble)
                        newList += expr
                    }
                    else if(toExpr(0).take(1) == "\'" || toExpr(0).take(1) == "\""){
                        val expr = Str(toExpr(0))       // delete the quotes then append
                        newList += expr
                    }
                    else{
                        val expr = Var(toExpr(0))
                        newList += expr
                    }
                }
                else{
                    val operator = toExpr(1)
                    val oprand1 = toExpr(0)
                    val oprand2 = toExpr(2)
                    val expr = getExpr(operator, oprand1, oprand2)
                    newList += expr
                }
            }
            val listExpr = newList.toList               // listExpr is the list of Expr objects
            val stmt = Print(listExpr)
            return stmt
    	}
    	else{
    		return Error("error")
    	} 
    	
    }

    def sysExit(lineNum: Double, stmt: Stmt): Unit = {
        stmt match{
                case Error(str) =>{
                    println("Syntax error on line " + lineNum.toInt.toString + ".")
                    System.exit(1)
                }
                case _ =>{
                    //do nothing
                }
            }
    }

    def parseStmt(tokenizedLine: Array[String], lineNum: Double, symTab: Map[String, Double], sList: ListBuffer[Stmt]): Unit ={
    	if(tokenizedLine(0).takeRight(1) == ":"){
    		val lengthList = tokenizedLine.length				// # tokens in the line
    		var label = tokenizedLine(0).dropRight(1)	// label is stored without the collection
    		updateSym(label, lineNum, symTab)
    		var keyword = tokenizedLine(1)
    		val expr = tokenizedLine.takeRight(lengthList - 2)	// expr stores the expression
    		val stmt = getStmt(keyword, expr)          // get convert the line into a stmt object
            sysExit(lineNum, stmt)
    		sList += stmt										// append the stmt to list of stmts -> sList
    	}
    	else{
    		val lengthList = tokenizedLine.length
    		val keyword = tokenizedLine(0)
    		val expr = tokenizedLine.takeRight(lengthList - 1)
    		val stmt = getStmt(keyword, expr)
            sysExit(lineNum, stmt)
    		sList += stmt
    	}
    }

    def perform(symTab: Map[String, Double], lineNum: Double, stmt: Stmt): Double = {
    	stmt match{
    		case Input(variable) =>{
    			val userInput = scala.io.StdIn.readDouble()
    			updateSym(variable, userInput, symTab)
    		}
    		case If(expr, label) =>{
    			if (eval(expr, symTab, lineNum) == 1.0) {
    				return symTab(label)
    			}
    		}
    		case Let(variable, expr) => {
    			val evaluated = eval(expr, symTab, lineNum)
    			updateSym(variable, evaluated, symTab)
    		} 
    		case Print(exprList) =>{
    			for(elem <- exprList){
    				elem match{
    					case Str(name) => {
    						val updatedStr = name.substring(1, name.length - 1)
    						print(updatedStr + " ")
    					}
    					case _ =>{
    						val evaluated = eval(elem, symTab, lineNum)
    						print(evaluated.toString + " ")
    					}
    				}
    			}
    			println()
    		}
    	}
    	return lineNum + 1
    }

    def run(lineNum: Double, sList: ListBuffer[Stmt], symTab: Map[String, Double]): Unit = {
    	if(lineNum == -1231231380.222222000999343222342){

        }
        if(lineNum <= sList.length){
    		run(perform(symTab, lineNum, sList(lineNum.toInt-1)), sList, symTab)
    	}
    }

    def main(args: Array[String]) = {
    	var lineNum: Double = 1.0
    	var sList = ListBuffer[Stmt]()				// will hold the list of stmts
    	val symTable = Map[String, Double]()		// symbol table will store label-lineNum mappings as well as var-val mappings	
    	var tokenizedLine = Array[String]()

    	val inputFile = args(0)
    	for (line <- Source.fromFile(inputFile).getLines){
            var temp1 = line.replaceAll("\t", " ")
    		tokenizedLine = temp1.split(" +")		// tokenizedLine is an array of tokens (one line)
            var temp2 = tokenizedLine.mkString(" ").trim()
    		parseStmt(temp2.split(" "), lineNum, symTable, sList)
    		lineNum += 1
    	}
    	// execute run here, after building the list of stmts and filling the 
    	// symbol table with label-lineNum mappings
        run(1.0, sList, symTable)
    }
}