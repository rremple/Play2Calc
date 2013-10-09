package controllers

import play.api._
import play.api.mvc._

import views._

import models._

object Application extends Controller {

  /**
   * Display about.
   */
  def about = Action {
    Ok(html.about());
  }

}