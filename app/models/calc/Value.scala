package models.calc

/*
 * A value is a quantity along with its Units. The default, where units are an empty map, represents a scalar.
 */
case class Value(number: Double, units: Units) {
  def *(other: Value) = Value(number * other.number, units * other.units)
  def /(other: Value) = Value(number / other.number, units / other.units)
  def +(other: Value) = Value(number + other.number, units)
  def -(other: Value) = Value(number - other.number, units)
  def ^(other: Double) = Value(math.pow(number, other), units ^ other)

  override def toString = {
    val formatDouble = (d: Double) => if (d.intValue.doubleValue() == d) d.toInt.toString else d.toString
    val unitsString = units.toString
    formatDouble(number) + (if (unitsString.isEmpty) "" else " " + unitsString)
  }
}
case object Value {
  def apply(n: Double, u: String): Value = Value(n, Units(Map(u -> 1.0)))
  def apply(n: Double): Value = Value(n, Units.scalar)
}