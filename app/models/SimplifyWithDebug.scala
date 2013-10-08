package models

import scala.util.parsing.combinator.RegexParsers

class SimplifyWithDebug(val conversionsToSi: Map[String, Value] = Map()) extends RegexParsers {

  /*
   * Use parser-combinators for syntax-directed translation. This implementation gives separate syntax rules
   * for value expressions and units expressions, and then they are combined using multiplication.
   */
  val debug = !true // simple switch and helper function to produce logging or non-logging parsers
  def p[A](parser: Parser[A], debugString: String) = if (debug) log(parser)(debugString) else parser

  def conversions = p((unitsString ~ ("=" ~> value)).*, "c --> [us = v]...") ^^ {
    case convRep => { convRep.map { _ match { case unitsString ~ value => (unitsString -> value) } }.toMap }
  }

  def calculationExpression = p(valueExpression ~ ("in" ~> unitsExpression).?, "ce --> ve[in ue]") ^^ {
    case value ~ Some(inUnits) => value / inUnits.siUnitValue(conversionsToSi) * Value(1.0, inUnits)
    case value ~ None => value
  }

  def valueExpression = p(valueFactor ~ (("+" | "-") ~ valueFactor).*, "ve --> vf [op vf]...") ^? (
    { // Partial function matches only if all the units are equal...
      case x ~ opRep if opRep.forall(_ match { case _ ~ y => x.units.equals(y.units) }) =>
        opRep.foldLeft(x)((x, subExpr) => subExpr match {
          case "+" ~ y => x + y
          case "-" ~ y => x - y
        })
    }, { // ... but if not, return an error string including any mismatching units
      case x ~ opRep => opRep.map(_ match { case op ~ y => y.units })
        .filter(!_.equals(x.units))
        .map("'" + _ + "'")
        .mkString("Units of '" + x.units + "' are incompatible with: ", ", ", ".")
    }
  )

  def valueFactor = p(valueTerm ~ (("*" | "/") ~ valueTerm).*, "vf --> vt [op vt]...") ^^ {
    case x ~ opRep => {
      opRep.foldLeft(x)((x, subExpr) => subExpr match {
        case "*" ~ y => x * y
        case "/" ~ y => x / y
      })
    }
  }

  def valueTerm = p(simpleValueTerm ~ ("^" ~> numberAsValue).?, "vt --> svt[^n]") ^^ {
    case x ~ Some(exp) => x ^ exp.number
    case x ~ None => x
  }

  def simpleValueTerm: Parser[Value] = (
    p(value, "svt --> v") |
    p(numberAsValue, "svt --> n") |
    p("(" ~> valueExpression <~ ")", "svt --> (ve)")
  )

  def value = p(numberAsValue ~ unitsExpression, "v --> n ue") ^^ {
    case x ~ units => x * units.siUnitValue(conversionsToSi)
  }

  def unitsExpression = p(unitsTerm ~ (("*" | "/") ~ unitsTerm).*, "ue --> ut [op ut]...") ^^ {
    case x ~ opRep => opRep.foldLeft(x)((x, subExpr) => subExpr match {
      case "*" ~ y => x * y
      case "/" ~ y => x / y
    })
  }

  def unitsTerm = p(simpleUnitsTerm ~ ("^" ~> numberAsValue).?, "ut --> sut[^n]") ^^ {
    case x ~ Some(exp) => x ^ exp.number
    case x ~ None => x
  }

  def simpleUnitsTerm: Parser[Units] = p(units, "sut --> u") | p("(" ~> unitsExpression <~ ")", "sut --> (ue)")

  def units: Parser[Units] = p(unitsString, "u --> us")  ^^ { s => Units(s) }

  def unitsString: Parser[String] = p("""[A-Za-z]+""".r, "us")

  def numberAsValue: Parser[Value] = p("""[+-]?\d+(\.\d*)?""".r, "n") ^^ { s => Value(s.toDouble) }

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