import java.io.{BufferedReader, InputStreamReader}
import java.util.concurrent.CountDownLatch

import akka.actor.Actor.Receive
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import akka.routing.RoundRobinPool
import akka.stream.ActorMaterializer
import akka.stream.ActorMaterializerSettings
import org.apache.http.HttpResponse
import org.apache.http.client.methods.HttpGet
import org.apache.http.concurrent.FutureCallback
import org.apache.http.impl.nio.client.CloseableHttpAsyncClient
import org.apache.http.impl.nio.client.HttpAsyncClients

import scala.util.parsing.json.JSON

case class WikiCategory(start: Int, end: Int, url: String) {
  def extract(httpclient: CloseableHttpAsyncClient): Result = {
    val latch: CountDownLatch = new CountDownLatch(end - start + 1);
    val index = start
    val futures = for (
      index <- start until end
    ) yield new HttpGet(s"$url/$index")


    // Questions Part 1:
    // I understand that an asynchronous call works
    // on an event queue and loop, where normally (in Node)
    // a thread pool will pull from the event queue
    // and work on that job. That thread may continue
    // until the job is done and will then throw
    // future callback on the main thread.

    // 1) Is there some inherent thread pool used in Akka
    // that scala futures use and if so, is it bad design to use
    // FutureCallback from org.apache.http.concurrent?

    // 2) Is this the best use of futures, will we run out of memory
    // and should we instead batch process our future maps?
    futures.map(
        req => httpclient.execute(req, new FutureCallback[HttpResponse] {
          override def cancelled(): Unit = {
            latch.countDown()
          }


          // This callback function will be responsible for parsing
          // the html content.

          // Questions Part 2:
          // 1) Should we reduce and return our HTML parsed content
          // back to our sender and let the master be the only writter
          // in the system? Or is it ok to simply write to our distributed
          // / sharded database directly from our AKKA actor in this
          // function?
          override def completed(result: HttpResponse): Unit = {
            latch.countDown()
            println(s"result -> ${req.getURI.getPath} -> ${result.getStatusLine()}")
          }

          override def failed(ex: Exception): Unit = {
            latch.countDown()
          }
        }
      )
    )

    latch.await()
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

  var httpclient: CloseableHttpAsyncClient = _

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
    val system = ActorSystem("WikiSystem")

    // Our listener is just a reference actor
    // which we need for integration testing
    val listener = system.actorOf(Props[Listener])
    val master = system.actorOf(
      Props(
        new Master(listener)
      ),
      name = "master"
    )
    master ! readFile()
  }

  // We receive a list of urls and begin performing work extracting the
  // information from our html page.
  class Worker extends Actor {
    val httpclient = HttpAsyncClients.createDefault()
    // Each actor will have one initiated httpclient
    // connection started
    httpclient.start()
    override def receive: Receive = {
      // Simply take our batched url and extract
      // our webpage contents
      case wc: WikiCategory =>
        sender ! wc.extract(httpclient)
    }
  }

  // This is just filler, we might
  // do something with this later
  class Listener extends Actor {
    def receive = {
      case _ =>
    }
  }

  // Master actor initiates our 6 other Actors
  class Master(ref: ActorRef) extends Actor {
    val start: Long = System.currentTimeMillis()

    val workerRouter = context.actorOf(
      Props[Worker].withRouter(RoundRobinPool(6)),
      name = "workerRouter"
    )

    var nrOfResults = 0
    val nrOfWorkers = 6

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
    def distributeWork(urls: Option[List[Map[String, Any]]]):Unit = {
      urls match {
        case Some(_urls: List[Map[String, Any]]) =>
          _urls.foreach {
            url =>
              // Now we go ahead and create our workers
              // and get stuff rolling

              // For each url we have
              // url:
              // size:
              // We want to break size into 6 parts

              val size: Int = url("size").asInstanceOf[Double].toInt
              val msize = math.ceil(size.toDouble / nrOfWorkers).toInt
              for (i <- 0 until nrOfWorkers) {
                val start = (i * msize) + 1
                val end = math.min(((i + 1) * msize), size)
                workerRouter ! WikiCategory(start, end, url("url").asInstanceOf[String])
              }
          }
        case None =>
      }
    }

    override def receive: Receive = {
      case urls: Option[List[Map[String, Any]]] =>
        distributeWork(urls)
      case result: Result =>
        ref ! result
      case _ => println("Recieved something")
    }
  }
}