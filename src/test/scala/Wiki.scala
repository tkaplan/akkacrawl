import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestActors, TestKit, TestProbe}
import org.scalatest.WordSpecLike
import org.scalatest.Matchers
import org.scalatest.BeforeAndAfterAll
import wikiClient.Master

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
    "Expect the follow" in {
      masterRef ! Option(df)
      prob.receiveN(12).map({
        result => {
          val _result = result.asInstanceOf[Result]
          _result.url match {
            case "https://wikileaks.org/clinton-emails/emailid/" =>
              _result.start match {
                case 1 => assert(_result.end == 5158)
                case 5159 => assert(_result.end == 10316)
                case 10317 => assert(_result.end == 15474)
                case 15475 => assert(_result.end == 20632)
                case 20633 => assert(_result.end == 25790)
                case 25791 => assert(_result.end == 30945)
                case _ =>
                  fail
              }
            case "https://wikileaks.org/podesta-emails/emailid/" =>
              _result.start match {
                case 0 => println("yes")
                case _ =>
              }
            case _ => println("Did get a matching url")
          }
        }
      })
    }
  }
}