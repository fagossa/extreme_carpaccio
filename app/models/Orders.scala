package models

import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.language.implicitConversions
import scala.math.BigDecimal.RoundingMode

abstract class DiscountType(val name: String) {
  def apply(value: BigDecimal): BigDecimal
}

object Standard extends DiscountType(name = "STANDARD") {
  def apply(value: BigDecimal): BigDecimal = value match {
    case response if response >= 50000 => value * 0.15
    case response if response >= 10000 => value * 0.10
    case response if response >= 7000 => value * 0.07
    case response if response >= 5000 => value * 0.05
    case response if response >= 1000 => value * 0.03
    case _ => 0
  }

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

  private def integratePriceAndQuantity: Option[List[(BigDecimal, Int)]] =
    if (prices.size == quantities.size) {
      Some(prices.zip(quantities))
    } else {
      None
    }

  private def applyTaxes(tuples: List[(BigDecimal, Int)]): BigDecimal = {
    tuples.map { tuple =>
      val price = Some(tuple._1)
      val qty = Some(tuple._2)

      for {
        maybePercent <- getTax
        maybePrice <- price
        maybeQty <- qty
      } yield {
        val rawTotal = maybePrice * maybeQty
        rawTotal * (1 + maybePercent)
      }
    }.flatten.sum
  }

  def calculateResponse: Option[OrderResponse] = {
    for {
      groupedData <- integratePriceAndQuantity
    } yield {
      val withTaxes = applyTaxes(groupedData)
      val total = withTaxes - reduction.apply(withTaxes)
      OrderResponse(total.setScale(2, RoundingMode.HALF_UP))
    }
  }



  def getTax: Option[Double] = country match {
    case "FI" => Some(0.17)
    case "SK" => Some(0.18)
    case "CZ" | "ES" => Some(0.19)
    case "DE" | "FR" | "RO" | "NL" | "EL" | "LV" | "MT" => Some(0.20)
    case "UK" | "BG" | "DK" | "IE" | "CY" | "PL" => Some(0.21)
    case "EE" | "AT" => Some(0.22)
    case "PT" | "SE" | "HR" | "LT" => Some(0.23)
    case "BE" | "SI" => Some(0.24)
    case "LU" | "IT" => Some(0.25)
    case "HU" => Some(0.27)
    case _ => None
  }

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

