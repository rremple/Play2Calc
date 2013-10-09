package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import views._
import models._
import models.calc.GCalc
import models.CalcParameters

object Calculator extends Controller {

  val calculatorForm = Form[CalcParameters](
    mapping(
      "conversions" -> text,
      "expression" -> text
    )(CalcParameters.apply)(CalcParameters.unapply))

  val defaultParams = CalcParameters("""
ft = 0.3048 m
in = 0.0254 m
cm = 0.01 m
mi = 1609.344 m
min = 60.0 s
hr = 3600.0 s""", ""
  )

  /**
   * Display a form with default conversions and no expression.
   */
  def form = Action {
    Ok(html.calculator(calculatorForm.fill(defaultParams), Right(None)));
  }

  /**
   * Handle form submission
   */
  def submit = Action { implicit request =>
    calculatorForm.bindFromRequest.fold(
      errors => BadRequest(html.calculator(errors, Left(List("Bad request")))),
      params => Ok(html.calculator(calculatorForm.fill(params), evaluationOf(params)))
    )
  }

  private def evaluationOf(params: CalcParameters): Either[List[String], Option[String]] = {
    (new GCalc).parseConversions(params.conversions).fold(
      errors => Left(List("Unable to parse conversion rules", errors)),
      rules => ((new GCalc(rules)).evaluate(params.expression)).fold(
        errors => Left(List("Unable to evaluate expression", errors)),
        result => Right(Some(result.toString()))
      )
    )
  }

}