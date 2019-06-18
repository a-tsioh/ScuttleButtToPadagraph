package sbtp

import io.circe.Json
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

import sbtp.MessageTypes._

import sys.process._

object Fetcher {

  def getFeed(size: Int): LazyList[String] = s"""/opt/pierre/node-v10.16.0-linux-x64/bin/node
/opt/pierre/node-v10.16.0-linux-x64/bin/ssb-server
createLogStream
--limit $size
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
    readStream(getFeed(10000))
      .flatten
      .map(MessageTypes.parseMsgJson)
      .map(_.value.content)
      .filter { case _: UnhandledContent => false case _ => true }
      .take(50)
      .foreach(println)
  }


}
