package models.parse

class Rewriter(debug: Boolean = false) {

  /*
   * Helpers
   */
  private def lambdaFromBody(body: List[Statement]) = Function(UniqueName.get, Nil, body)
  private def lambdaFromBody(name: String, body: List[Statement]) = Function(name, Nil, body)
  private def statementFromCall(expression: Expression) = ExpressionStatement(FunctionCall(expression, Nil))
  private lazy val noOp = lambdaFromBody(Nil)

  /*
   * Rewriters
   */
  private def rewrite(expression: Expression): Expression = expression match {
    case Operate(operator, ex1, ex2) => Operate(operator, rewrite(ex1), rewrite(ex2))
    case Compare(comparator, ex1, ex2) => Compare(comparator, rewrite(ex1), rewrite(ex2))
    case FunctionCall(function, params) => FunctionCall(rewrite(function), params.map(rewrite))
    case Function(name, paramIds, body) => Function(name, paramIds, body.map(rewrite))
    case IfElseExpression(ifEx, thenEx, elseEx) => IfElseExpression(rewrite(ifEx), rewrite(thenEx), rewrite(elseEx))
    case NullValue() => NullValue()
    case LiteralValue(value) => LiteralValue(value)
    case VariableValue(name) => VariableValue(name)
  }

  private def rewrite(statement: Statement): Statement = statement match {
    case ExpressionStatement(expression) => ExpressionStatement(rewrite(expression))
    case VariableAssignment(variable, value) => VariableAssignment(variable, rewrite(value))
    case VariableDeclaration(variable, value) => VariableDeclaration(variable, rewrite(value))
    case PrintStatement(expression) => PrintStatement(rewrite(expression))
    case ErrorStatement(expression) => ErrorStatement(rewrite(expression))

    case IfElseStatement(ifEx, thenBody, elseBody) =>
      statementFromCall(IfElseExpression(
        rewrite(ifEx),
        lambdaFromBody(thenBody.map(rewrite)),
        lambdaFromBody(elseBody.map(rewrite))
      ))

    case WhileStatement(whileEx, body) => {
      val bodyFunctionName = UniqueName.get

      val ifTrueDoStatement = statementFromCall(IfElseExpression(
        rewrite(whileEx),
        VariableValue(bodyFunctionName),
        noOp
      ))

      statementFromCall(lambdaFromBody(List(
        VariableDeclaration(
          bodyFunctionName,
          lambdaFromBody(bodyFunctionName, body.map(rewrite) :+ ifTrueDoStatement)
        ),
        ifTrueDoStatement
      )))
    }

    case ForStatement(variable, expression, body) => {
      val bodyFunctionName = UniqueName.get
      val iteratorFunctionName = UniqueName.get

      val declareIteratorStatement = VariableDeclaration(iteratorFunctionName, rewrite(expression))
      val callIteratorExpression = FunctionCall(VariableValue(iteratorFunctionName), Nil)
      val declareVariableStatement = VariableDeclaration(variable, callIteratorExpression)
      val assignVariableStatement = VariableAssignment(variable, callIteratorExpression)
      val ifNotNullDoStatement = statementFromCall(IfElseExpression(
        Compare("!=", VariableValue(variable), NullValue()),
        VariableValue(bodyFunctionName),
        noOp
      ))
      val declareBodyFunctionStatement = VariableDeclaration(
        bodyFunctionName,
        lambdaFromBody(
          bodyFunctionName,
          body.map(rewrite) ++ List(assignVariableStatement, ifNotNullDoStatement)
        )
      )

      statementFromCall(lambdaFromBody(
        List(
          declareIteratorStatement,
          declareBodyFunctionStatement,
          declareVariableStatement,
          ifNotNullDoStatement
        )
      ))
    }
  }

  def rewrite(input: List[Statement]): List[Statement] = input.map(rewrite)
}
object Rewriter {
  def apply(debug: Boolean = false) = new Rewriter(debug)
}