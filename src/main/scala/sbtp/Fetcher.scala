package sbtp

import com.github.tototoshi.csv.CSVWriter
import io.circe.Json
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
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

  def collectFeeds(ll: LazyList[Msg[ParsedContent]]): Set[String] = {
    ll.flatMap {msg =>
      msg.value.author :: (msg.value.content match {
        case AboutContent(a,_,_,_,_) => List(a)
        case p: PostContent =>
          p.mentions.map(_.map(_.link).filter(_.startsWith("@"))).getOrElse(Array.empty).toList ::: p.reply.map(_.values.toList).getOrElse(Nil)
        case c: ContactContent => List(c.contact)
        case _ => Nil
      })
    }
      .toSet
  }

  def getNames(msgStream: LazyList[Msg[ParsedContent]]): Map[String, List[String]] = {
    msgStream
      .filter(_.value.content.isInstanceOf[AboutContent])
      .map(m => m.value.content)
      .collect {case AboutContent(_,about, Some(name),_,_)  => (about, name)}
      .foldLeft(Map.empty[String,List[String]].withDefaultValue(Nil)) {case (mappings, (feed,name)) =>
          mappings.updated(feed, name :: mappings(feed))
      }
  }

  def getDescriptions(msgStream: LazyList[Msg[ParsedContent]]): Map[String, List[String]] = {
    msgStream
      .filter(_.value.content.isInstanceOf[AboutContent])
      .map(m => m.value.content)
      .collect {case AboutContent(_,about, _,_,Some(desc))  => (about, desc)}
      .foldLeft(Map.empty[String,List[String]].withDefaultValue(Nil)) {case (mappings, (feed,desc)) =>
        mappings.updated(feed, desc :: mappings(feed))
      }
  }

  def getFollowLinks(msgStream: LazyList[Msg[ParsedContent]]): List[(String, String)] = {
    msgStream
      .filter(_.value.content.isInstanceOf[ContactContent])
      .map {m =>  (m.value.author, m.value.content) }
      .flatMap {
        case (a,ContactContent(_, who, following,af,pub)) =>
          if(af.contains(true) || pub.contains(true)) None
          else Some((a,who))
      } // TODO: deal with following = false
      .distinct
      .toList
  }


  case class Feed(id: String, names: List[String], follows: List[String]) {
    val label: String = names.headOption.getOrElse("UNK")
  }

  case class Post(id: String, author: String, content: String, related: Set[String]) {
    val label: String = content.take(10)
  }

  def collectPosts(msgStream: LazyList[Msg[ParsedContent]]): List[Post] = {
    msgStream
      .filter(_.value.content.isInstanceOf[PostContent])
      .map {m => (m.key, m.value.author, m.value.content)}
      .collect { case (id, author, content:PostContent) =>
          val links = content.branch.getOrElse(Array.empty).toList ::: content.root.toList
          Post(id.drop(1), author.drop(1), content.text, links.toSet.map {s: String => s.drop(1)})
      }
      .toList
  }


  def main(args: Array[String]): Unit = {
    //exploreUnhandledType("contact")
    val ll = readStream(getFeed()).flatten.map(MessageTypes.parseMsgJson)
    //exploreUnhandledType(ll, "contact")

    val feeds = collectFeeds(ll)
    val names = getNames(ll)
    //val links = getFollowLinks(ll)
    val posts = collectPosts(ll)



    val nodes = feeds.map {id => Feed(id, names(id), Nil)}//, links.filter(_._1 == id).map(_._2.drop(1)))}

    val writer = CSVWriter.open("/tmp/graph_posts.csv")
    writer.writeRow(Seq("@Feeds: #ID", "label", "+names", "%+follows", "shape"))
    for(n <- nodes) {
      writer.writeRow(Seq(n.id.drop(1), n.label, n.names mkString " ; ", n.follows  mkString ";", "square"))
    }
    writer.writeRow(Seq("@Posts: #ID", "label", "%author", "text", "%+ related"))
    for(p <- posts) {
      writer.writeRow(Seq(p.id, p.label, p.author, p.content, p.related.mkString(" ; ")))
    }

    writer.close()


  }

  def exploreUnhandledType(ll: LazyList[Msg[ParsedContent]], t: String): Unit = {
    ll
      .map(_.value.content)
      .collect {case c: UnhandledContent => c.json}
      .filter(_.hcursor.downField("type").as[String].contains(t))
      .take(500)
      .flatMap(json => json.asObject.map(_.keys.toList).getOrElse(Nil))
      .distinct
      .sorted
      .foreach(println)
  }

  def sampleType(ll: LazyList[Msg[ParsedContent]], t: String): Unit = {
    ll
      .filter(_.value.content.t == t)
      .take(10)
      .foreach(println)
  }

  def confirmAllHandled(ll: LazyList[Msg[ParsedContent]], t: String): Unit = {
    ll
      .filter(_.value.content.t == t)
      .map(_.value.content)
      .collect {case c: UnhandledContent => c}
      .foreach(println)

  }

  //def collectIDs()

  def checkStuff(): Unit = {
    readStream(getFeed(Some(1000)))
      .flatten
      .map(MessageTypes.parseMsgJson)
      //.map(_.value.content)
      .filter { _.value.content.isInstanceOf[PostContent] }
      .map(_.value.content)
      .collect {case c: PostContent => c}
      .take(50)
      .foreach(println)
  }


}
