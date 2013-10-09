package models.parse

object Imp {

  case class X(dashes: String) {
    def addDash = X(dashes+"-")
  }

  def x(p: Int)(implicit i: X) = {println(p.toString() + i.dashes)}

}