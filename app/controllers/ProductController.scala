package controllers

import models.{FeedBack, Orders}
import play.api.libs.json.Json
import play.api.mvc.{Controller, _}

import scala.concurrent.Future

object ProductController extends Controller {

  def receiveOrder = Action.async(BodyParsers.parse.json) { implicit request =>
    request.body.asOpt[Orders] match {
      case Some(orders) =>
        println(s" @receiveOrder : $orders")
        orders.calculateResponse match {
          case Some(response) =>
            Future.successful(Ok(Json.toJson(response)))
          case None =>
            Future.successful(BadRequest("Invalid content"))
        }

      case None =>
        Future.successful(BadRequest("Invalid content"))
    }


  }

  def feedback = Action.async(BodyParsers.parse.json) { implicit request =>
    request.body.asOpt[FeedBack] match {
      case Some(feed) =>
        println(s" @feedback : $feed")
        Future.successful(Ok(Json.toJson(feed)))

      case None =>
        Future.successful {
          BadRequest("Invalid content")
        }
    }

  }


}
