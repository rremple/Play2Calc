package models.parse

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers

/*
 * Use parser-combinators to build the AST. Using a PackratParser because it supports left recursion.
 * Note that the 'debug' parameter is a simple switch for using all logging or all non-logging parsers.
 */
class Parser(debug: Boolean = false) extends RegexParsers with PackratParsers {

  /*
   * Helper function to produce logging or non-logging parsers
   */
  def p[A](parser: Parser[A], debugString: String) = if (debug) log(parser)(debugString) else parser

  /*
   * Programs
   */
  lazy val P = p(S.*, "List of statements")

  /*
   * Statements
   */
  lazy val S: Parser[Statement] = p(aFunctionDefinition | aVariableDeclaration |
    aPrintStatement | anErrorStatement | anIfElseStatement | aWhileLoopStatement | aForLoopStatement |
    aVariableAssignment | anExpressionStatement, "S")

  lazy val aFunctionDefinition = p("def" ~> id ~ anIdList ~ aStatementBody, "Function Definition") ^^ {
    case id ~ params ~ body => VariableDeclaration(id, Function(UniqueName.get, params, body))
  }
  lazy val aVariableDeclaration = p("def" ~> id ~ ("=" ~> E), "Var. Declaration") ^^ {
    case id ~ e => VariableDeclaration(id, e)
  }
  lazy val aPrintStatement = p("print" ~> E, "Print E to console") ^^ { e => PrintStatement(e) }

  lazy val anErrorStatement = p("error" ~> E, "Print E and exit") ^^ { e => ErrorStatement(e) }

  lazy val anIfElseStatement = p("if" ~> ("(" ~> E <~ ")") ~ aStatementBody ~ ("else" ~> aStatementBody), "If-else statement") ^^ {
    case cond ~ thenBody ~ elseBody => IfElseStatement(cond, thenBody, elseBody)
  }
  lazy val aWhileLoopStatement = p("while" ~> ("(" ~> E <~ ")") ~ aStatementBody, "While loop") ^^ {
    case cond ~ body => WhileStatement(cond, body)
  }
  lazy val aForLoopStatement = p("for" ~> ("(" ~> id) ~ ("in" ~> E <~ ")") ~ aStatementBody, "For loop") ^^ {
    case id ~ in ~ body => ForStatement(id, in, body)
  }
  lazy val aVariableAssignment: Parser[Statement] = p(id ~ ("=" ~> E), "Variable Assignment") ^^ {
    case id ~ e => VariableAssignment(id, e)
  }
  lazy val anExpressionStatement = p(E, "Expression") ^^ { e => ExpressionStatement(e) }

  lazy val aStatementBody = p("{" ~> S.* <~ "}", "a body of statements")

  lazy val anIdList = p("(" ~> id.* <~ ")", "a list of ids")

  /*
   * Expressions
   */
  lazy val E: PackratParser[Expression] = p(anAnonymousFunction | anIteConditional | ("(" ~> E <~ ")") |
    anOperator | aComparator | aFunctionCall | aNumberLiteral | theNullValue | aVariable, "E")

  lazy val anAnonymousFunction = p("lambda" ~> ("(" ~> id.* <~ ")") ~ aStatementBody, "Anonymous function") ^^ {
    case params ~ body => Function(UniqueName.get, params, body)
  }
  lazy val anIteConditional = p(("ite" ~> "(" ~> E) ~ ("," ~> E) ~ ("," ~> E <~ ")"), "Ite Conditional") ^^ {
    case i ~ t ~ e => IfElseExpression(i, t, e)
  }
  lazy val anOperator = p(E ~ ("+" | "-" | "*" | "/") ~ E, "Addition, Subtraction, Multiplication, or Integer Division") ^^ {
    case e1 ~ numOp ~ e2 => Operate(numOp, e1, e2)
  }
  lazy val aComparator = p(E ~ ("==" | "!=" | "<=" | ">=" | "<" | ">") ~ E, "Equality, Inequality, LessThan, GreaterThan Tests") ^^ {
    case e1 ~ boolOp ~ e2 => Compare(boolOp, e1, e2)
  }
  lazy val aFunctionCall = p(E ~ ("(" ~> E.* <~ ")"), "Function Call") ^^ {
    case function ~ params => FunctionCall(function, params)
  }
  lazy val aNumberLiteral = p(num, "A number literal") ^^ { num => LiteralValue(num) }

  lazy val theNullValue = p("null", "The null value") ^^ { _ => NullValue() }

  lazy val aVariable = p(id, "A variable") ^^ { id => VariableValue(id) }

  /*
   * RegEx's
   */
  lazy val num = p("""-?[0-9]+""".r, "A Number") ^^ { s => s.toDouble }

  lazy val id = p("""[a-zA-Z_][a-zA-Z_0-9]*""".r, "An Identifier")

  def parse(input: String): Either[String, List[Statement]] = parseAll(P, input) match {
    case Success(result, _) => Right(result)
    case failure: NoSuccess => Left(failure.msg)
  }
}
object Parser {
  def apply(debug: Boolean = false) = new Parser(debug)
}