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

  var configureArray: Array[mutable.Stack[ifStreamCondition]] = _

  case class ifStreamCondition(id: Option[String], ele: Option[String], clazz: Option[String], key: Option[String])
  case class IfTextMap(text: String, id: String)
  case class DomElement(ifTextMap: IfTextMap)

  var domStack = mutable.Stack[Option[DomElement]]()

  def configure(configures: Array[mutable.Stack[ifStreamCondition]]): Unit = {
    this.configureArray = configures
  }

  def compareIf(compare: ifStreamCondition): Array[ifStreamCondition] = {
    def condition(ifStack: mutable.Stack[ifStreamCondition]): Boolean = {
      if (ifStack.isEmpty) return false
      val ifO = ifStack.top
      ifO.id match {
        case id =>
          if (!id.equals(compare.id))
            return false
      }
      ifO.ele match {
        case ele =>
          if (!ele.equals(compare.ele))
            return false
      }
      ifO.clazz match {
        case Some(clazz) =>
          val clazzList = compare.clazz.getOrElse("").replaceAll("\\s+", " ").trim.split(" ")
          var flag = false
          clazzList.foreach((clazz) => flag = flag || (clazz.toLowerCase == clazz.toLowerCase))
          return flag
      }

      return true
    }

    val whichStack:Array[ifStreamCondition] = for (
      ifStack <- this.configureArray if condition(ifStack)
    ) yield ifStack.pop()

    return whichStack
  }

  def process(dataString:String, dataMap: Map[String, String]): (String, Map[String, String]) = {
      var _dataMap = dataMap
      return extractNextTag findFirstIn dataString match {
        case Some(tag) =>
          return eleType findFirstIn tag match {
            case Some(tagname) =>
              // Lets extract our text
              val text = extractText.findFirstIn(extractNextTag.replaceFirstIn(dataString, "[--PIPE--]")).getOrElse("")
              val _dataString = dataString.substring(text.size, dataString.size)

              // Lets map our new text to our ifStack
              domStack.map((dom) =>
                dom match {
                  case Some(dom) =>
                    DomElement(IfTextMap(dom.ifTextMap.text + text, dom.ifTextMap.id))
                  case _ => None
                }
              )

              if (Constants.isSelfClosing(tagname)) {
                return process(extractNextTag.replaceFirstIn(_dataString, ""), _dataMap)
              }

              // Before seeing if our new element fits our ifconditions
              // we must see what kind of tag it is

              // On close tag, we must pop our ifStack and build our map
              if (isClosingTag.findFirstIn(tag).equals(Some(tag))) {
                // Is this dom element associated with an if condition?
                domStack.pop() match {
                  case Some(dom) =>
                    _dataMap = _dataMap ++ Map(dom.ifTextMap.id -> dom.ifTextMap.text)
                  case _ =>
                }

                return process(extractNextTag.replaceFirstIn(_dataString, ""), _dataMap)
              }

              // If it's not a closing or self closing tag, then it must be an open tag

              // Now lets see if our new element fits our ifconditions
              val compare = ifStreamCondition(
                extractFromQuotes.
                  findFirstIn(
                    extractId.findFirstIn(tagname).getOrElse("")
                  ),
                Option(tagname),
                extractFromQuotes.
                  findFirstIn(
                    extractClass.findFirstIn(tagname).getOrElse("")
                  ),
                None
              )

              val ifMatch = compareIf(compare)

              if (ifMatch.length > 0) {
                // Our new element fits the bill, then lets add it to our
                // ifStack
                ifMatch(0).key match {
                  case Some(key) => domStack.push(Some(DomElement(IfTextMap("",key))))
                  case None => domStack.push(None)
                }

              } else {
                domStack.push(None)
              }
              return (_dataString, _dataMap)
          }
        case None => return (dataString, dataMap)
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
