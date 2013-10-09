package models.parse

class Interpreter(debug: Boolean = false) {

  type ValueType = Either[Lambda, Option[Double]]
  val noValue: ValueType = Right(None)

  class Console {
    var lines: List[String] = Nil
    def clear: Unit = { lines = Nil }
    def print(out: String): Unit = { lines = out :: lines }
    def print(tag: String, v: ValueType): Unit =
      print(tag + v.fold(lambda => "<lambda>", num => num.map(_.toString) getOrElse "null"))
    def get = lines.reverse
  }
  val console = new Console

  case class Lambda(params: List[String], body: List[Statement], callFrame: CallFrame)

  case class CallFrame(
      enclosingFrame: Option[CallFrame] = None,
      dictionary: scala.collection.mutable.Map[String, ValueType] = scala.collection.mutable.Map(),
      var returnValue: ValueType = noValue) {

    override def toString = "CallFrame(" + enclosingFrame.map(_ => "Enclosed") + "," + returnValue + ")"

    private def findDictionaryFrame(name: String): Option[CallFrame] = {
      //if (debug) println("  looking for " + name)
      if (dictionary.get(name).isDefined) Some(this) else enclosingFrame.flatMap(_.findDictionaryFrame(name))
    }

    def setReturn(value: ValueType) = {
      returnValue = value
      this
    }

    def resetReturn = setReturn(noValue)

    def setInDictionary(name: String, value: ValueType) = {
      if (debug) println("\n  setting " + name + " = " + value)
      findDictionaryFrame(name).getOrElse(this).dictionary += (name -> value)
      this
    }

    def getFromDictionary(name: String): ValueType = {
      val value = findDictionaryFrame(name).getOrElse(this).dictionary.get(name).getOrElse(noValue)
      if (debug) println("\n  got " + name + " = " + value)
      value
    }

    def newEnclosedFrame() = {
      if (debug) println("\n  creating new enclosed frame")
      CallFrame(enclosingFrame = Some(this))
    }
  }

  private def evaluate(callFrame: CallFrame, expression: Expression): ValueType = expression match {

    case NullValue() => {
      if (debug) println("\n*I* Ex: NullValue")
      noValue
    }

    case LiteralValue(value) => {
      if (debug) println("\n*I* Ex: LiteralValue(" + value + ")")
      Right(Some(value))
    }

    case VariableValue(name) => {
      if (debug) println("\n*I* Ex: VariableValue(" + name + ")")
      callFrame.getFromDictionary(name)
    }

    case Operate(operator, ex1, ex2) => {
      if (debug) println("\n*I* Ex: Operate(" + operator + "," + ex1 + "," + ex2 + ")")
      (operator, evaluate(callFrame, ex1), evaluate(callFrame, ex2)) match {
        case ("+", Right(Some(x)), Right(Some(y))) => Right(Some(x + y))
        case ("-", Right(Some(x)), Right(Some(y))) => Right(Some(x - y))
        case ("*", Right(Some(x)), Right(Some(y))) => Right(Some(x * y))
        case ("/", Right(Some(x)), Right(Some(y))) => Right(Some(x / y))
        case _                                     => noValue
      }
    }

    case Compare(comparator, ex1, ex2) => {
      if (debug) println("\n*I* Ex: Compare(" + comparator + "," + ex1 + "," + ex2 + ")")
      (comparator, evaluate(callFrame, ex1), evaluate(callFrame, ex2)) match {

        case ("==", Left(x), Left(y)) if x == y => Right(Some(1))
        case ("==", Right(Some(x)), Right(Some(y))) if x == y => Right(Some(1))
        case ("==", Right(None), Right(None)) => Right(Some(1))

        case ("!=", Left(x), Left(y)) if x != y => Right(Some(1))
        case ("!=", Right(Some(x)), Right(Some(y))) if x != y => Right(Some(1))
        case ("!=", Right(Some(_)), Right(None)) => Right(Some(1))
        case ("!=", Right(None), Right(Some(_))) => Right(Some(1))
        case ("!=", Right(_), Left(_)) => Right(Some(1))
        case ("!=", Left(_), Right(_)) => Right(Some(1))

        case ("<=", Right(Some(x)), Right(Some(y))) if x <= y => Right(Some(1))
        case (">=", Right(Some(x)), Right(Some(y))) if x >= y => Right(Some(1))
        case ("<", Right(Some(x)), Right(Some(y))) if x < y => Right(Some(1))
        case (">", Right(Some(x)), Right(Some(y))) if x > y => Right(Some(1))

        case _ => Right(Some(0))
      }
    }

    case FunctionCall(function, params) => {
      if (debug) println("\n*I* Ex: FunctionCall(" + function + "," + params + ")")
      evaluate(callFrame, function) match {
        case Left(lambda) => {
          val lambdaFrame = lambda.params.zip(params).foldLeft(lambda.callFrame) {
            case (frame, (name, expression)) => frame.setInDictionary(name, evaluate(callFrame, expression))
          }
          evaluate(lambdaFrame.resetReturn, lambda.body).returnValue
        }
        case Right(x) => {
          throw new RuntimeException("***** Can't call a non-lambda:" + x)
        }
      }
    }

    case Function(name, paramIds, body) => {
      if (debug) println("\n*I* Ex: Function(" + name + "," + paramIds + "," + body + ")")
      callFrame.getFromDictionary(name) match {
        case Left(lambda) => Left(lambda)
        case Right(None) => {
          val newLambda = Left(Lambda(paramIds, body, callFrame.newEnclosedFrame()))
          callFrame.setInDictionary(name, newLambda) // breaks eval pattern with explicit side-effect
          newLambda
        }
        case Right(Some(x)) => {
          throw new RuntimeException("***** Lambda's identifier being used for:" + x)
        }
      }
    }

    case IfElseExpression(ifEx, thenEx, elseEx) => {
      if (debug) println("\n*I* Ex: IfElseExpression(" + ifEx + "," + thenEx + "," + elseEx + ")")
      val thenEval = evaluate(callFrame, thenEx)
      val elseEval = evaluate(callFrame, elseEx)
      evaluate(callFrame, ifEx) match {
        case Right(Some(x)) if x != 0 => thenEval
        case _                        => elseEval
      }
    }
  }

  private def evaluate(callFrame: CallFrame, statement: Statement): CallFrame = statement match {

    case ExpressionStatement(expression) => {
      if (debug) println("\n*I* St: StatementExpression(" + expression + ")")
      callFrame.setReturn(evaluate(callFrame, expression))
    }

    case VariableAssignment(variable, value) => {
      if (debug) println("\n*I* St: VariableAssignment(" + variable + "," + value + ")")
      callFrame.setInDictionary(variable, evaluate(callFrame, value)).resetReturn
    }

    case VariableDeclaration(variable, value) =>
      {
        if (debug) println("\n*I* St: VariableDeclaration(" + variable + "," + value + ")")
        callFrame.setInDictionary(variable, evaluate(callFrame, value)).resetReturn
      }

    case PrintStatement(expression) => {
      if (debug) println("\n*I* St: PrintStatement(" + expression + ")")
      console.print("  >>> ", evaluate(callFrame, expression));
      callFrame.resetReturn
    }

    case ErrorStatement(expression) => {
      if (debug) println("\n*I* St: ErrorStatement(" + expression + ")")
      console.print("  Error >>> ", evaluate(callFrame, expression));
      callFrame.resetReturn
    }

    case IfElseStatement(ifEx, thenBody, elseBody) => {
      //      if (debug) println("\n*I* St: IfElseStatement(" + ifEx + "," + thenBody + "," + elseBody + ")")
      //      evalExpression(callFrame, ifEx) match {
      //        case Right(Some(x)) if x > 0 => evaluate(callFrame, thenBody)
      //        case _ => evaluate(callFrame, elseBody)
      //      }
      throw new RuntimeException("***** should have been rewritten: " + IfElseStatement(ifEx, thenBody, elseBody))
    }

    case WhileStatement(expression, body) => {
      //      if (debug) println("\n*I* St: WhileStatement(" + expression + "," + body + ")")
      //      evalExpression(callFrame, expression) match {
      //        case Right(Some(x)) if x > 0 => evalStatement(evaluate(callFrame, body), WhileStatement(expression, body))
      //        case _ => callFrame
      //      }
      throw new RuntimeException("***** should have been rewritten: " + WhileStatement(expression, body))
    }

    case ForStatement(variable, expression, body) => {
      //      if (debug) println("\n*I* St: ForStatement(" + variable + "," + expression + "," + body + ")")
      //
      //      def repeatUntilNullReturned(localCallFrame: CallFrame, lambda: Lambda): CallFrame =
      //        evaluate(lambda.callFrame.resetReturn, lambda.body).returnValue match {
      //          case Right(Some(value)) => {
      //            val newCallFrame = localCallFrame.setInDictionary(variable, Right(Some(value)))
      //            repeatUntilNullReturned(evaluate(newCallFrame, body), lambda)
      //          }
      //          case Right(None) => localCallFrame // normal halt
      //          case x => { println("***** Unexpected return value from iterator lambda:" + x); localCallFrame }
      //        }
      //
      //      evaluate(callFrame, expression) match {
      //        case Left(lambda) => repeatUntilNullReturned(callFrame, lambda)
      //        case x => { ("***** Iterator expression did not return a lambda:" + x); callFrame }
      //      }
      throw new RuntimeException("***** should have been rewritten: " + ForStatement(variable, expression, body))
    }
  }

  def evaluate(callFrame: CallFrame, statements: List[Statement]): CallFrame = statements.foldLeft(callFrame)(evaluate)

  def interpret(statements: List[Statement]) = {
    console.clear
    val result = evaluate(CallFrame(), Rewriter(debug).rewrite(statements)).returnValue
    val value = result.fold(lambda => { console.print("result was a lambda!"); None }, num => num)
    (statements, console.get, value)
  }
}
object Interpreter {
  def apply(debug: Boolean = false) = new Interpreter(debug)
}

