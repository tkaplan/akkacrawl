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

import scala.collection.immutable.HashMap
import scala.concurrent.Future
import scala.util.Try
import scala.util.parsing.json.JSON

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

//    // Our listener is just a reference actor
//    // which we need for integration testing
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

    def printResponses(response:(Try[HttpResponse],String)): Unit = {
      println(s"Status: ${response._1.get.status}, Number: ${response._2}")
      response.
        _1.
        get.
        entity.
        dataBytes.
        reduce(_.decodeString("UTF-8") + _).
        runWith(Sink.ignore)
    }

    val seeds = paths.map(path => HttpRequest(uri=path) -> path)

    val g = RunnableGraph.fromGraph(GraphDSL.create() { implicit builder: GraphDSL.Builder[NotUsed] =>
      import GraphDSL.Implicits._
      val balancer = builder.add(Balance[(HttpRequest, String)](6, waitForAllDownstreams = false))
      val out = Sink.ignore
      val in = Source(seeds)
      in ~> balancer.in
      for (i <- Range(0,6)) {
        balancer.out(i) ~> wikiFlow.async ~> Flow[(Try[HttpResponse], String)].map(printResponses) ~> out
      }

      ClosedShape
    })

    g.run()(materializer)
  }

  // We receive a list of urls and begin performing work extracting the
  // information from our html page.
  class Worker extends Actor {
    // Each actor will have one initiated httpclient
    // connection started
    override def receive: Receive = {
      // Simply take our batched url and extract
      // our webpage contents
      case wc: List[WikiCategory] =>
    }
  }

  // This is just filler, we might
  // do something with this later
  class Listener extends Actor {
    def receive = {
      case _ =>
    }
  }

  trait Distributor {
    val nrOfWorkers = 0
    def distributeWork(urls: Option[List[Map[String, Any]]]):Map[String, List[WikiCategory]] = {
      var batch = Map[String, List[WikiCategory]]()
      urls match {
        case Some(_urls: List[Map[String, Any]]) =>
          _urls.foreach {
            url:Map[String, Any] =>
              val size: Int = url("size").asInstanceOf[Double].toInt
              val msize: Int = math.ceil(size.toDouble / nrOfWorkers).toInt
              val wikiList:List[WikiCategory] = (for (
                i <- 0 until nrOfWorkers
              ) yield {
                val start = (i * msize) + 1
                val end = math.min(((i + 1) * msize), size)
                WikiCategory(start, end, url("url").asInstanceOf[String])
              }).toList
              batch += (url("url").asInstanceOf[String] -> wikiList)
          }
        case None =>
      }
      return batch
    }
  }

  // Master actor initiates our 6 other Actors
  class Master(ref: ActorRef) extends Actor with Distributor {
    val start: Long = System.currentTimeMillis()
    val workerRouter = context.actorOf(
      Props[Worker].withRouter(RoundRobinPool(6)),
      name = "workerRouter"
    )

    var nrOfResults = 0
    override val nrOfWorkers = 6

    // This a shitty function and couples business logic
    // too hard to make this unit testable
    // Will fix in the future.

    // This simply takes each url:
    // clinton-emails, podesta-emails, dnc-emails
    // and distributes the work among 6 actors.

    // For example, clinton-emails have around 30945 emails total
    // and for 6 actors, that creates a batch size of ~5058 per actor.
    // So we tell each actor to crawl 5058 emails and store them in
    // our distributed db.

    // We don't actually care what those actors return, even if that
    // defeats the appeal of akka.
    override def receive: Receive = {
      case urls: Option[List[Map[String, Any]]] =>
        distributeWork(urls)
        urls match {
          case Some(_urls: List[Map[String, Any]]) =>
            _urls.foreach {
              url:Map[String, Any] =>
                val size: Int = url("size").asInstanceOf[Double].toInt
                val source: Source[String, NotUsed] = Source((1 to size).map(
                  index => {
                    s"${url("url")}/$index"
                  }
                ))
            }
          case None =>
        }
      case result: Result =>
        ref ! result
      case _ => println("Recieved something")
    }
  }
}