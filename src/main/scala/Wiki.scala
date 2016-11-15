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

    futures.map(
        req => httpclient.execute(req, new FutureCallback[HttpResponse] {
          override def cancelled(): Unit = {
            latch.countDown()
          }
          override def completed(result: HttpResponse): Unit = {
            latch.countDown()
            println(s"result -> ${req.getURI.getPath} -> ${result.getStatusLine()}")

//
//            var r:BufferedReader = new BufferedReader(new InputStreamReader(result.getEntity().getContent()));
//
//            var line:String = null;
//
//            while ((line = r.readLine()) != null) {
//              line match {
//                case x: String => println(x)
//                case _ =>
//              }
//            }
//            r.close();
          }
          override def failed(ex: Exception): Unit = {
            latch.countDown()
          }
        }
      )
    )

    latch.await()

    new Result(start, end, url)
  }
}

case class Result(start: Int, end: Int, url: String)

object wikiClient extends App {
  main()

  var httpclient: CloseableHttpAsyncClient = _

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

  def main() {
    val system = ActorSystem("WikiSystem")
    val listener = system.actorOf(Props[Listener])
    val master = system.actorOf(
      Props(
        new Master(listener)
      ),
      name = "master"
    )
    master ! readFile()
  }

  class Worker extends Actor {
    val httpclient = HttpAsyncClients.createDefault()
    httpclient.start()
    override def receive: Receive = {
      case wc: WikiCategory =>
        sender ! wc.extract(httpclient)
    }
  }

  class Listener extends Actor {
    def receive = {
      case _ =>
    }
  }

  class Master(ref: ActorRef) extends Actor {
    val start: Long = System.currentTimeMillis()

    val workerRouter = context.actorOf(
      Props[Worker].withRouter(RoundRobinPool(6)),
      name = "workerRouter"
    )

    var nrOfResults = 0
    val nrOfWorkers = 6

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