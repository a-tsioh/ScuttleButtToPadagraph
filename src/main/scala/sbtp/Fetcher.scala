package sbtp

import io.circe.Json
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

import sbtp.MessageTypes._

import sys.process._

object Fetcher {

  def getFeed(size: Option[Int]=None): LazyList[String] = s"""/opt/pierre/node-v10.16.0-linux-x64/bin/node
/opt/pierre/node-v10.16.0-linux-x64/bin/ssb-server
createLogStream
${size.map(i => s"--limit $i").getOrElse("")} --reverse
""".replace("\n", " ").lazyLines_!


  def readStream(toRead: LazyList[String], buffer: List[String]=Nil): LazyList[Option[Msg[Json]]] = {
    (toRead, buffer) match {
      case ("" +: tl,Nil) => readStream(tl, buffer)
      case ("" +: tl, buf) => parse(buf.reverse mkString "\n") match {
        case Right(js) => js.as[Msg[Json]].toOption #:: readStream(tl, Nil)
        case Left(_) => readStream(tl, Nil)
      }
      case (line +: tl, buf) => readStream(tl, line :: buf)
      case (l, _) if l.isEmpty => LazyList.empty
    }
  }


  def main(args: Array[String]): Unit = {
    readStream(getFeed())
      .flatten
      .map(MessageTypes.parseMsgJson)
      //.map(_.value.content)
      .filter { _.value.content.isInstanceOf[PostContent] }
      .map(_.value.content)
      .collect {case c: PostContent => c}
      .filter {_.channel.contains("mmmmm")}
      .take(50)
      .foreach(println)
  }


}
