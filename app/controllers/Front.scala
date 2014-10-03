package controllers

import play.api._
import play.api.mvc._

object Front extends Controller {

  def Main = Action {
    Ok(views.html.pos.pos())
  }

}