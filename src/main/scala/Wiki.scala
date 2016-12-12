import java.io.{BufferedReader, InputStreamReader}
import java.util.concurrent.CountDownLatch

import akka.{Done, NotUsed}
import akka.actor.Actor.Receive
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import akka.routing.RoundRobinPool
import akka.stream._
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import akka.stream.impl.fusing.Scan
import akka.stream.scaladsl.{Balance, Broadcast, Flow, GraphDSL, Keep, Merge, RunnableGraph, Sink, Source}
import akka.util.ByteString

import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}
import scala.util.parsing.json.JSON
import scala.concurrent.ExecutionContext.Implicits.global

case class WikiCategory(start: Int, end: Int, url: String) {
  def extract(): Result = {

    // Questions Part 3:
    // As mentioned in questions part 2, we can either send results
    // back to our master sender or simply write to the database
    // directly and return OK to our sender actor.
    new Result(start, end, url)
  }
}

case class Result(start: Int, end: Int, url: String)

object wikiClient extends App {
  main()

  implicit val system = ActorSystem("Akka")

  // This simply reads our json file in wiki_urls.json
  // and returns our json as List[Map[String, Any]]
  def readFile(name: String = "/wiki_urls.json"): Option[List[Map[String, Any]]] = {
    println(name)
    val resource = getClass.getResource(name)
    val json = JSON.parseFull(scala.
      io.
      Source.
      fromFile(resource.getFile()).
      mkString)

    return json match {
      case Some(root: Map[String, Any]) =>
        root("urls") match {
          case urls: List[Map[String, Any]] =>
            return Option(urls)
          case _ => None
        }
      case _ => None
    }
  }

  // Simply start our program
  def main() {
    implicit val system = ActorSystem("WikiSystem")

    // Our listener is just a reference actor
    // which we need for integration testing
    implicit val materializer = ActorMaterializer()
    implicit val wikiFlow = Http().cachedHostConnectionPoolHttps[String]("wikileaks.org")
    implicit val urls = readFile()
    implicit val paths:List[String] = urls match {
      case Some(urls: List[Map[String,Any]]) =>
        urls flatMap {
          url:Map[String, Any] =>
            Range(1,url.get("size").get.asInstanceOf[Double].toInt).map(i => s"${url.get("path").get}/$i")
        }
      case None => List()
    }

    def parseHtml(response:(Try[HttpResponse],String)): Unit = {
      var domStack = new mutable.Stack[String]
      println(s"Status: ${response._1.get.status}, Number: ${response._2}")

      /*
        We have several types of tags we must consider ---

        <tag>  --- implicit self ending with no <some />
                -- These are edge cases we must handle with
                   a dictionary of known self ending tags.
                   These include area, base, br, col, command,
                   embed, hr, img, input, keygen, link, meta,
                   param, source, track, wbr
        <tag/> --- explicit self ending with /
        <tag></tag> --- stack tags

        We want to create a map object that will be used for
        data analytics. The structure is the following:

        {
          "subject": [string],
          "from": [string],
          "to": Array[string],
          "cc": Array[string],
          "date": [string]
        }
       */

      object wikiHtmlParsable extends WikiHtmlParsable {

      }

      wikiHtmlParsable.configure(Array(
        mutable.Stack(
          wikiHtmlParsable.ifStreamCondition(
            id = Some("content"),
            ele = Some("div"),
            None,
            key = Some("email")
          )
        )
      ))

      val datamap:Future[(String, Map[String, String])] = response.
        _1.
        get.
        entity.
        dataBytes.
        runFold[(String, Map[String, String])](("", Map()))(wikiHtmlParsable.htmlStreamReduce)

      datamap.foreach((a) => {
        println(a._2)
      })
    }

    val seeds = paths.map(path => HttpRequest(uri=path) -> path)

    val g = RunnableGraph.fromGraph(GraphDSL.create() { implicit builder: GraphDSL.Builder[NotUsed] =>
      import GraphDSL.Implicits._
      val balancer = builder.add(Balance[(HttpRequest, String)](6, waitForAllDownstreams = false))
      val out = Sink.ignore
      val in = Source(seeds)
      in ~> balancer.in
      for (i <- Range(0,6)) {
        balancer.out(i) ~> wikiFlow.async ~> Flow[(Try[HttpResponse], String)].map(parseHtml) ~> out
      }

      ClosedShape
    })

    g.run()(materializer)
  }
}