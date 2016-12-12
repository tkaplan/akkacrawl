import akka.util.ByteString
import org.scalatest._

import scala.collection.mutable

class WikiHtmlParsableSpec() extends FlatSpec with Matchers with BeforeAndAfterAll {

  val htmlDoc = Array[ByteString](
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
            <div id="content">
            please"""),
    ByteString(""" work
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

  wikiHtmlParsable.configure(Array(
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
        Some("test2")
      )
    )
  ))

  override def afterAll: Unit = {  }

  override def beforeAll: Unit = {  }

  "getNewDataStringAndText" should "return these" in {
    val a =
      """
          I
          am
          awesome
          <test>yes
      """
    val (text, _dataString) = wikiHtmlParsable.getNewDataStringAndText(a)
    assert(text == " I am awesome ")
    assert(_dataString == "<test>yes ")
  }

  "mapToTextMap" should "return these" in {
    val a = "this is a com"
    val b = "plete message"
    val stack:mutable.Stack[Option[wikiHtmlParsable.DomElement]] = mutable.Stack[Option[wikiHtmlParsable.DomElement]]()
    stack.push(Some(
      wikiHtmlParsable.DomElement(
        wikiHtmlParsable.IfTextMap(
          "",
          "test"
        )
      )))
    stack.push(None)
    val test = wikiHtmlParsable.mapToTextMap(stack, a)
    test.foreach(
      x =>
       x match {
         case Some(dom) =>
           assert(dom.ifTextMap.id == "test")
           assert(dom.ifTextMap.text == a)
         case None =>
      }
    )
    val test2 = wikiHtmlParsable.mapToTextMap(test, b)
    test2.foreach(
      x =>
        x match {
          case Some(dom) =>
            assert(dom.ifTextMap.id == "test")
            assert(dom.ifTextMap.text == a + b)
          case None =>
        }
    )
  }

  "wikiHtmlParsable for htmlDoc2" should "return Map(email-content -> This is a test)" in {
    val mapTest = wikiHtmlParsable.reduceHtml(htmlDoc)
    assert(mapTest("test") == " we  win  ")
    assert(mapTest("test2") == " please work ")
  }
}