package models

import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.language.implicitConversions
import scala.math.BigDecimal.RoundingMode

abstract class DiscountType(val name: String) {
  def apply(value: BigDecimal): BigDecimal
}

object Standard extends DiscountType(name = "STANDARD") {
  def apply(value: BigDecimal): BigDecimal = ???

}

object PayThePrice extends DiscountType(name = "PAY THE PRICE") {
  def apply(value: BigDecimal): BigDecimal = value
}

object DiscountType {

  val discountTypes: Seq[DiscountType] = Seq(Standard, PayThePrice)

  implicit val discountTypeFormat = new Format[DiscountType] {
    def reads(js: JsValue): JsResult[DiscountType] = js match {
      case JsString(s) => discountTypes.find(_.name == s) map { dt =>
        JsSuccess(dt)
      } getOrElse JsError("Invalid discount type: " + js.toString)
      case _ => JsError("Invalid discount type: " + js.toString)
    }

    def writes(dt: DiscountType): JsValue = JsString(dt.toString)
  }
}

case class OrderResponse(total: BigDecimal)

object OrderResponse {

  implicit val readsOrder: Reads[OrderResponse] = Json.reads[OrderResponse]

  implicit val writesOrder: Writes[OrderResponse] = Json.writes[OrderResponse]

}

case class Orders(prices: List[BigDecimal], quantities: List[Int], country: String, reduction: DiscountType) {

  def calculateResponse: Option[OrderResponse] = ???

  def getTax: Option[Double] = ???

}


object Orders {

  implicit val readsOrder: Reads[Orders] = {
    (
      (__ \ "prices").read[List[BigDecimal]] and
        (__ \ "quantities").read[List[Int]] and
        (__ \ "country").read[String] and
        (__ \ "reduction").read[DiscountType]
      )(Orders.apply _)
  }

  implicit val writesOrder: Writes[Orders] = {
    (
      (__ \ "prices").write[List[BigDecimal]] and
        (__ \ "quantities").write[List[Int]] and
        (__ \ "country").write[String] and
        (__ \ "reduction").write[DiscountType]
      )(unlift(Orders.unapply))
  }

}

