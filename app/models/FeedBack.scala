package models

import play.api.libs.json.{Json, Reads, Writes}

case class FeedBack(`type`: String, content: String)

object FeedBack {
  implicit val readsOrder: Reads[FeedBack] = Json.reads[FeedBack]

  implicit val writesOrder: Writes[FeedBack] = Json.writes[FeedBack]
}
