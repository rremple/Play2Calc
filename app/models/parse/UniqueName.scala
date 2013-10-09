package models.parse

object UniqueName {
  var x = 0
  def get = {
    x += 1
    "_lambda_" + x
  }
}
