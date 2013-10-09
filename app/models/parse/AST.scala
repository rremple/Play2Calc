package models.parse

trait AST

sealed abstract class Statement extends AST
case class ExpressionStatement(e: Expression) extends Statement
case class VariableAssignment(variable: String, value: Expression) extends Statement
case class VariableDeclaration(variable: String, value: Expression) extends Statement
case class PrintStatement(e: Expression) extends Statement
case class ErrorStatement(e: Expression) extends Statement
case class IfElseStatement(e: Expression, thenBody: List[Statement], elseBody: List[Statement]) extends Statement
case class WhileStatement(e: Expression, body: List[Statement]) extends Statement
case class ForStatement(variable: String, in: Expression, body: List[Statement]) extends Statement

sealed abstract class Expression extends AST
case class NullValue() extends Expression
case class LiteralValue(value: Double) extends Expression
case class VariableValue(name: String) extends Expression
case class Operate(name: String, e1: Expression, e2: Expression) extends Expression
case class Compare(name: String, e1: Expression, e2: Expression) extends Expression
case class Function(function: String, params: List[String], body: List[Statement]) extends Expression
case class FunctionCall(function: Expression, params: List[Expression]) extends Expression
case class IfElseExpression(ifPart: Expression, thenPart: Expression, elsePart: Expression) extends Expression