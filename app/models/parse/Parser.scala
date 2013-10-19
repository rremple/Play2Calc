package models.parse

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers

/*
 * Use parser-combinators to build the AST. Using a PackratParser because it supports left recursion.
 * Note that the 'debug' parameter is a simple switch for using all logging or all non-logging parsers.
 * 
 * Language defined here: https://sites.google.com/a/bodik.org/cs164/directory/pa1#TOC-The-Language-cs164-
 */
class Parser(debug: Boolean = false) extends RegexParsers with PackratParsers {

  /*
   * Helper function to produce logging or non-logging parsers
   */
  def ::=[A](parser: Parser[A], debugString: String) = if (debug) log(parser)(debugString) else parser

  /*
   * Programs
   */
  lazy val P = ::=(anSRep.*, "List of statements")

  lazy val anSRep = ::=(S <~ ";".?, "statements may be separated by semicolons or whitespace")

  /*
   * Statements
   */
  lazy val S: Parser[Statement] = ::=(aFunctionDefinition | aVariableDeclaration |
    aPrintStatement | anErrorStatement | anIfElseStatement | aWhileLoopStatement | aForLoopStatement |
    aVariableAssignment | anExpressionStatement, "S")

  lazy val aFunctionDefinition = ::=("def" ~> id ~ anIdList ~ aStatementBody, "Function Definition") ^^ {
    case id ~ params ~ body => VariableDeclaration(id, Function(UniqueName.get, params, body))
  }
  lazy val aVariableDeclaration = ::=("def" ~> id ~ ("=" ~> E), "Var. Declaration") ^^ {
    case id ~ e => VariableDeclaration(id, e)
  }
  lazy val aPrintStatement = ::=("print" ~> E, "Print E to console") ^^ { e => PrintStatement(e) }

  lazy val anErrorStatement = ::=("error" ~> E, "Print E and exit") ^^ { e => ErrorStatement(e) }

  lazy val anIfElseStatement = ::=("if" ~> anExpressionArg ~ aStatementBody ~ ("else" ~> aStatementBody), "If-else statement") ^^ {
    case cond ~ thenBody ~ elseBody => IfElseStatement(cond, thenBody, elseBody)
  }
  lazy val aWhileLoopStatement = ::=("while" ~> anExpressionArg ~ aStatementBody, "While loop") ^^ {
    case cond ~ body => WhileStatement(cond, body)
  }
  lazy val aForLoopStatement = ::=("for" ~> ("(" ~> id) ~ ("in" ~> E <~ ")") ~ aStatementBody, "For loop") ^^ {
    case id ~ in ~ body => ForStatement(id, in, body)
  }
  lazy val aVariableAssignment: Parser[Statement] = ::=(id ~ ("=" ~> E), "Variable Assignment") ^^ {
    case id ~ e => VariableAssignment(id, e)
  }
  lazy val anExpressionStatement = ::=(E, "Expression") ^^ { e => ExpressionStatement(e) }

  lazy val aStatementBody = ::=("{" ~> P <~ "}", "a list of statements -- same as a program")

  lazy val anIdList = ::=("(" ~> anIdRep.* <~ ")", "a list of ids")

  lazy val anIdRep = ::=(id <~ ",".?, "ids may be separated by commas or whitespace")

  /*
   * Expressions
   */
  lazy val E: PackratParser[Expression] = ::=(anAnonymousFunction | anIteConditional | anExpressionArg |
    anOperator | aComparator | aFunctionCall | aNumberLiteral | theNullValue | aVariable, "E")

  lazy val anAnonymousFunction = ::=("lambda" ~> anIdList ~ aStatementBody, "Anonymous function") ^^ {
    case params ~ body => Function(UniqueName.get, params, body)
  }
  lazy val anIteConditional = ::=(("ite" ~> "(" ~> E) ~ ("," ~> E) ~ ("," ~> E <~ ")"), "Ite Conditional") ^^ {
    case i ~ t ~ e => IfElseExpression(i, t, e)
  }
  lazy val anExpressionArg = ::=("(" ~> E <~ ")", "an expression as an argument: (E)")

  lazy val anOperator = ::=(E ~ ("+" | "-" | "*" | "/") ~ E, "Addition, Subtraction, Multiplication, or Integer Division") ^^ {
    case e1 ~ numOp ~ e2 => Operate(numOp, e1, e2)
  }
  lazy val aComparator = ::=(E ~ ("==" | "!=" | "<=" | ">=" | "<" | ">") ~ E, "Equality, Inequality, LessThan, GreaterThan Tests") ^^ {
    case e1 ~ boolOp ~ e2 => Compare(boolOp, e1, e2)
  }
  lazy val aFunctionCall = ::=(E ~ ("(" ~> anERep.* <~ ")"), "Function Call") ^^ {
    case function ~ params => FunctionCall(function, params)
  }
  lazy val anERep = ::=(E <~ ",".?, "expressions as arguments may be separated by commas or whitespace")

  lazy val aNumberLiteral = ::=(num, "A number literal") ^^ { num => LiteralValue(num) }

  lazy val theNullValue = ::=("null", "The null value") ^^ { _ => NullValue() }

  lazy val aVariable = ::=(id, "A variable") ^^ { id => VariableValue(id) }

  /*
   * RegEx's
   */
  lazy val num = ::=("""-?[0-9]+""".r, "A Number") ^^ { s => s.toDouble }

  lazy val id = ::=("""[a-zA-Z_][a-zA-Z_0-9]*""".r, "An Identifier")

  def parse(input: String): Either[String, List[Statement]] = {
    UniqueName.reset
    parseAll(P, input) match {
      case Success(result, _) => Right(result)
      case failure: NoSuccess => Left(failure.msg)
    }
  }
}
object Parser {
  def apply(debug: Boolean = false) = new Parser(debug)
}