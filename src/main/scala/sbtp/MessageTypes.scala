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

  abstract class ParsedContent(val t: String)

  case class UnhandledContent(`type`: String,
                              json: Json) extends ParsedContent(`type`)


  case class AboutContent(`type`: String,
                          about: String,
                          name:Option[String],
                          image:Option[String],
                          description: Option[String]) extends ParsedContent(`type`)


  case class ChannelContent(`type`: String,
                            channel: String,
                            subscribed: Boolean) extends  ParsedContent(`type`)

  case class Mention(link: String,
                     name: Option[String])


  case class PostContent(`type`: String,
                         branch: Option[Array[String]],
                         root: Option[String],
                         fork: Option[String],
                         channel: Option[String],
                         mentions: Option[Array[Mention]],
                         recps: Option[String],
                         reply: Option[Map[String, String]],
                         text: String) extends ParsedContent(`type`)

  case class ContactContent(`type`: String,
                            contact: String,
                            following: Boolean,
                            autofollow: Option[Boolean],
                            pub: Option[Boolean]
                           ) extends  ParsedContent(`type`)



  def failedParse(m: Msg[Json], t: String): Msg[ParsedContent] = {
    m.copy[UnhandledContent](
      value = m.value.copy[UnhandledContent](content = UnhandledContent(t, m.value.content))
    )
  }

  def mapContent(m: Msg[Json],t: String): Msg[ParsedContent] = {
    (t match  {
      case "about" => m.value.content.as[AboutContent]
      case "channel" => m.value.content.as[ChannelContent]
      case "post" => // small hack to always get an array of strings for branches
        val x = (m.value.content.hcursor.downField("branch").as[String] match {
          case Right(s) => m.value.content.hcursor.downField("branch").set(Json.arr(s.asJson)).up.focus.getOrElse(m.value.content)
          case Left(_) => m.value.content
        })

        x.as[PostContent]
      case "contact" => m.value.content.as[ContactContent]
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
