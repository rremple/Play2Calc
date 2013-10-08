package models

/*
 * Units are expressed as a map of text strings to their related exponents, e.g., 
 * "square meters per second" encoded as "m^2/s" would be expressed as ("m" -> 2.0, "s" -> -1.0) 
 */
case class Units(unitToExponent: Map[String, Double]) {

  type Entry = Tuple2[String, Double]

  private val exponentIsZero = ((unit: String, exponent: Double) => exponent == 0).tupled

  private val exponentFor = (unit: String) => unitToExponent.getOrElse(unit, 0.0)

  def ^(power: Double) = Units(unitToExponent
    .map { case (unit, exponent) => unit -> exponent * power }
    .filterNot(exponentIsZero)
  )

  def *(other: Units) = Units((unitToExponent.keys.toSet ++ other.unitToExponent.keys)
    .map { unit => unit -> (exponentFor(unit) + other.exponentFor(unit)) }
    .filterNot(exponentIsZero)
    .toMap
  )

  def /(other: Units) = *(other ^ -1)

  override def toString = {
    val formatDouble = (d: Double) => if (d.intValue.doubleValue() == d) d.toInt.toString else d.toString
    val formatEntry = ((unit: String, exponent: Double) => {
      val absExponent = scala.math.abs(exponent)
      unit + (if (absExponent == 1) "" else "^" + (formatDouble(absExponent)))
    }).tupled
    val formatList = (acc: String, entry: Entry) => acc + (if (acc.isEmpty) "" else "*") + formatEntry(entry)
    
    val (numerators, denominators) = unitToExponent.span { case (_, exponent) => exponent > 0 }
    numerators.foldLeft("")(formatList) + {
      if (denominators.isEmpty) "" else "/" + {
        val denominatorString = denominators.foldLeft("")(formatList)
        if (denominators.size > 1) "(" + denominatorString + ")" else denominatorString
      }
    }
  }

  // Decompose, convert (to si), and then re-compose the unit value for these units
  def siUnitValue(conversionRules: Map[String, Value]) = unitToExponent
    .map { case (unit, exponent) => conversionRules.getOrElse(unit, Value(1.0, unit)) ^ exponent }
    .foldLeft(Value(1.0))((x, y) => x * y)
}
case object Units {
  def apply(u: String): Units = Units(Map(u -> 1.0))
  val scalar = Units(Map[String, Double]())
}
