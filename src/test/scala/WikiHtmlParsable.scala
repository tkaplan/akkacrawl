import akka.util.ByteString
import org.scalatest._

import scala.collection.mutable

class WikiHtmlParsableSpec() extends FlatSpec with Matchers with BeforeAndAfterAll {

  val htmlDoc1 = Array[ByteString](
    ByteString(
      """
          <!DOCTYPE html>
          <html>
          <head>
            <link stylesheet="blah">
            <meta ok>
            <meta this is a"""),
    ByteString(
      """>
            </head>
            <bo"""),
    ByteString(
      """dy>
            <!-- ok so far so good -->
            <div id="content" class="email-content">
              This is a test
              <div> Wootz </div>
            </div>
            </body>
            </html>
      """)
  )

  val htmlDoc2 = Array[ByteString](
    ByteString(
      """
          <!DOCTYPE html>
          <html>
          <head>
            <link stylesheet="blah">
            <meta ok>
            <meta this is a"""),
    ByteString(
      """>
            </head>
            <bo"""),
    ByteString(
      """dy>
            <!-- ok so far so good -->
            <div id="content-skip" class="email-content">
              This is a test
            </div>
            <div id="content2" class="email-content">
              This is a success
              <span class="test">
                we
                <div>
                  win
                </div>
              </span>
              <span>
                another test
              </span>
            </div>
            </body>
            </html>
      """)
  )

  object wikiHtmlParsable extends WikiHtmlParsable {

    def reduceHtml(html: Array[ByteString]): Map[String, String] = {
      return html.foldLeft[(String, Map[String, String])](("", Map()))(htmlStreamReduce)._2
    }
  }

  override def afterAll: Unit = {  }

  override def beforeAll: Unit = {  }

  "wikiHtmlParsable for htmlDoc1" should "return Map(email-content -> This is a test)" in {
    wikiHtmlParsable.configure(
      Array(
        mutable.Stack(
          wikiHtmlParsable.ifStreamCondition(
            id = Some("content2"),
            ele = Some("div"),
            None,
            None
          ),
          wikiHtmlParsable.ifStreamCondition(
            ele = Some("span"),
            clazz = Some("test"),
            id = None,
            key = Some("test")
          )
        ),
        mutable.Stack(
          wikiHtmlParsable.ifStreamCondition(
            id = Some("content"),
            None,
            None,
            Some("content")
          )
        )
      )
    )

    val mapTest1 = wikiHtmlParsable.reduceHtml(htmlDoc1)

    println(mapTest1)
  }
}