package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import views._
import models._
import models.parse.Parser
import models.parse.Interpreter
import models.parse.Statement
import models.ParseParameters

object Parse extends Controller {

  val parserForm = Form[ParseParameters](
    mapping(
      "input" -> text
    )(ParseParameters.apply)(ParseParameters.unapply))

  val defaultParams = ParseParameters("""
def fac(n) {
     if (n <= 1) {
         1
     } else {
         n * fac(n-1)
     }
} 
print fac(4)"""
  )

  val parser = new Parser
  val interpreter = new Interpreter

  /**
   * Display a form with default conversions and no expression.
   */
  def form = Action {
    Ok(html.parser(parserForm.fill(defaultParams), Right(None)));
  }

  /**
   * Handle form submission
   */
  def submit = Action { implicit request =>
    parserForm.bindFromRequest.fold(
      errors => BadRequest(html.parser(errors, Left(List("Bad request")))),
      params => Ok(html.parser(parserForm.fill(params), evaluationOf(params)))
    )
  }

  private def evaluationOf(params: ParseParameters): Either[List[String], Option[(List[Statement], List[String], Option[Double])]] = {
    parser.parse(params.input).fold(
      errors => Left(List("Unable to parse program: ", errors)),
      statements => Right(Some(interpreter.interpret(statements)))
    )
  }

}