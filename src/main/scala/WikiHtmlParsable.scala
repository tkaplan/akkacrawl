import akka.util.ByteString

import scala.collection.mutable

/**
  * Created by dev on 12/3/16.
  */
trait WikiHtmlParsable {
  val isOpeningTag = ("<[a-zA-Z]+(>|.*?[^?]>)").r
  val isClosingTag = ("(<\\/.+?>)").r
  val extractNextTag = ("(<.+?>)").r
  val extractText = ".+?(?=\\[--PIPE--\\])".r
  val eleType = ("(\\w+)").r
  val extractId = ("(id.+\\\"(.+)\\\")|(id.+\\'(.+)\\')").r
  val extractClass = ("(class.+\\\"(.+)\\\")|(class.+\\'(.+)\\')").r
  val extractFromQuotes = ("(?<=(\"|')).+(?=(\"|'))").r

  var configureStack = mutable.Stack[ifStreamCondition]()

  var configureArray: Array[ifStreamCondition] = _

  case class ifStreamCondition(id: String = _, ele: String = _, clazz: String = _, key: String = _)
  case class IfTextMap(text: String, id: String)

  var ifStack = mutable.Stack[IfTextMap]()

  def configure(configures: Array[ifStreamCondition]): Unit = {
    this.configureArray = configures
  }

  def flatTextExtractor(key: String): Unit = {

  }

  def compareIf(compare: ifStreamCondition): Boolean = {
    return false
  }

  def process(dataString:String, dataMap: Map[String, String]): Unit = {

      extractNextTag findFirstIn dataString match {
        case Some(tag) =>
          eleType findFirstIn tag match {
            case Some(tagname) =>
              // Lets extract our text
              val text = extractText.findFirstIn(extractNextTag.replaceFirstIn(dataString, "[--PIPE--]")).getOrElse("")
              val _dataString = dataString.substring(text.size, dataString.size)

              // Lets map our new text to our ifStack
              ifStack.map((ifO) => IfTextMap(ifO.text + text, ifO.id))

              if (Constants.isSelfClosing(tagname)) {
                return process(extractNextTag.replaceFirstIn(_dataString, ""), dataMap)
              }

              // Before seeing if our new element fits our ifconditions
              // we must see what kind of tag it is

              // On close tag, we must pop our ifStack and build our map

              // On open tag, we must see if we match any compare element

              // Now lets see if our new element fits our ifconditions
              val compare = ifStreamCondition(
                extractFromQuotes.
                  findFirstIn(
                    extractId.findFirstIn(tagname).getOrElse("")
                  ).getOrElse(""),
                tagname,
                extractFromQuotes.
                  findFirstIn(
                    extractClass.findFirstIn(tagname).getOrElse("")
                  ).getOrElse("")
              )

              if (compareIf(compare)) {
                // Our new element fits the bill, then lets add it to our
                // ifStack
                ifStack.push(IfTextMap("", compare.key))
              }
          }
        case None =>
      }
  }

  def htmlStreamReduce(data: (String, Map[String, String]), byteString: ByteString): (String, Map[String, String]) = {
    var dataString = byteString.decodeString("UTF-8") + data._1

    // Ok now lets run process which should
    // process our data string until there's
    // nothing else it can do
    return process(dataString, data._2)
  }
}
