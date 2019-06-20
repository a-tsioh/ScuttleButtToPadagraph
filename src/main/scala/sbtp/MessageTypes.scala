package sbtp

import io.circe.Json

import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

object MessageTypes {

  case class Msg[+C](key: String, value:MsgValue[C], timestamp: Long)
  case class MsgValue[+C](
                          previous: Option[String],
                          author: String,
                          sequence: Int,
                          timestamp: Long,
                          hash: String,
                          content: C,
                          signature: String)

  trait ParsedContent



  case class AboutContent(`type`: String, about: String, name:Option[String], image:Option[String], description: Option[String]) extends ParsedContent
  case class UnhandledContent(`type`: String, json: Json) extends ParsedContent
  case class ChannelContent(`type`: String, channel: String, subscribed: Boolean) extends  ParsedContent

  case class Mention(link: String, name: Option[String])
  case class PostContent(`type`: String,
                         branch: Option[String],
                         channel: Option[String],
                         mentions: Option[Array[Mention]],
                         recps: Option[String],
                         reply: JsonObject,
                         root: Option[String],
                         text: String) extends ParsedContent



  def failedParse(m: Msg[Json], t: String): Msg[ParsedContent] = {
    m.copy[UnhandledContent](
      value = m.value.copy[UnhandledContent](content = UnhandledContent(t, m.value.content))
    )
  }

  def mapContent(m: Msg[Json],t: String): Msg[ParsedContent] = {
    (t match  {
      case "about" => m.value.content.as[AboutContent]
      case "channel" => m.value.content.as[ChannelContent]
      case "post" => m.value.content.as[PostContent]
      case _ => Decoder.failedWithMessage("unknown type")(m.value.content.hcursor)
    })
      .toOption
      .map { ac => m.copy(value = m.value.copy(content = ac)) }
      .getOrElse(failedParse(m, t))
  }


  def parseMsgJson(m: Msg[Json]): Msg[ParsedContent] = {

    m
      .value
      .content
      .hcursor
      .downField("type")
      .as[String] match {
      case Left(_) =>failedParse(m, "TypeNotFound")
      case Right(t) => mapContent(m,t)
    }
  }


}
