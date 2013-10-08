package models

import scala.util.parsing.combinator.RegexParsers

/*
 * Use parser-combinators for syntax-directed translation. This implementation gives separate syntax rules
 * for value expressions and units expressions, and then they are combined using multiplication.
 * Inspired by the Google Calculator.
 */
class GCalc(conversionRules: Map[String, Value] = Map()) extends RegexParsers {

  def conversions = (unitsString ~ ("=" ~> value)).* ^^ {
    _.map { case unitsString ~ value => (unitsString -> value) }.toMap
  }

  def calculationExpression = valueExpression ~ ("in" ~> unitsExpression).? ^^ {
    case value ~ Some(inUnits) => value / inUnits.siUnitValue(conversionRules) * Value(1.0, inUnits)
    case value ~ None => value
  }

  def valueExpression = valueFactor ~ (("+" | "-") ~ valueFactor).* ^? (
    { // Partial function matches only if all the units are equal...
      case x ~ opRep if opRep.forall { case _ ~ y => x.units equals y.units } => opRep.foldLeft(x) {
        case (x, "+" ~ y) => x + y
        case (x, "-" ~ y) => x - y
      }
    },
    { // ... but if not, return an error string including any mismatching units
      case x ~ opRep => opRep
        .map { case op ~ y => y.units }
        .filterNot(x.units equals)
        .map("'" + _ + "'")
        .mkString("Units of '" + x.units + "' are incompatible with: ", ", ", ".")
    }
  )

  def valueFactor = valueTerm ~ (("*" | "/") ~ valueTerm).* ^^ {
    case x ~ opRep => opRep.foldLeft(x) {
      case (x, "*" ~ y) => x * y
      case (x, "/" ~ y) => x / y
    }
  }

  def valueTerm = simpleValueTerm ~ ("^" ~> numberAsValue).? ^^ {
    case x ~ Some(y) => x ^ y.number
    case x ~ None => x
  }

  def simpleValueTerm: Parser[Value] = value | numberAsValue | "(" ~> valueExpression <~ ")"

  def value = numberAsValue ~ unitsExpression ^^ {
    case x ~ units => x * units.siUnitValue(conversionRules)
  }

  def unitsExpression = unitsTerm ~ (("*" | "/") ~ unitsTerm).* ^^ {
    case x ~ opRep => opRep.foldLeft(x) {
      case (x, "*" ~ y) => x * y
      case (x, "/" ~ y) => x / y
    }
  }

  def unitsTerm = simpleUnitsTerm ~ ("^" ~> numberAsValue).? ^^ {
    case x ~ Some(y) => x ^ y.number
    case x ~ None => x
  }

  def simpleUnitsTerm: Parser[Units] = units | "(" ~> unitsExpression <~ ")"

  def units: Parser[Units] = unitsString ^^ { s => Units(s) }

  def unitsString: Parser[String] = """[A-Za-z]+""".r

  def numberAsValue: Parser[Value] = """[+-]?\d+(\.\d*)?""".r ^^ { s => Value(s.toDouble) }

  def parseConversions(rules: String): Either[String, Map[String, Value]] =
    parseAll(conversions, rules) match {
      case Success(result, _) => Right(result)
      case failure: NoSuccess => Left(failure.msg)
    }

  def evaluate(expression: String): Either[String, Value] =
    parseAll(calculationExpression, expression) match {
      case Success(result, _) => Right(result)
      case failure: NoSuccess => Left(failure.msg)
    }
}