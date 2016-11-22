import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestActors, TestKit, TestProbe}
import org.scalatest.WordSpecLike
import org.scalatest.Matchers
import org.scalatest.BeforeAndAfterAll
import wikiClient.{Distributor, Master}

import scala.concurrent.duration.Duration

class WikiSpec() extends TestKit(ActorSystem("WikiSystem")) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll {

  val df = List(
    Map(
      "url" -> "https://wikileaks.org/clinton-emails/emailid/",
      "size" -> 30945.0
    ),
    Map(
      "url" -> "https://wikileaks.org/podesta-emails/emailid/",
      "size" -> 45526.0
    )
  )

  var masterRef: ActorRef = _
  var prob: TestProbe = _

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  override def beforeAll: Unit = {
    prob = TestProbe()
    masterRef = system.actorOf(Props(classOf[Master], prob.ref))
  }

  "Master" must {
    "Trait Distributor" in {
      object master extends Distributor {
        override val nrOfWorkers = 6
      }

      // We are unit testing our business logic in our
      // distributor
      master.
        distributeWork(Option(df)).
        foreach(result => {
          if (result._1 == "https://wikileaks.org/clinton-emails/emailid/") {
            result._2.foreach(wiki =>
              wiki.start match {
              case 1 => assert(wiki.end == 5158)
              case 5159 => assert(wiki.end == 10316)
              case 10317 => assert(wiki.end == 15474)
              case 15475 => assert(wiki.end == 20632)
              case 20633 => assert(wiki.end == 25790)
              case 25791 => assert(wiki.end == 30945)
              case _ =>
                fail
            })
          }
        })
    }
  }
}