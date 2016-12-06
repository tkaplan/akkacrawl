/**
  * Created by dev on 12/3/16.
  */
object Constants {
  val selfClosingTags = Set[String](
    "area", "base", "br", "col", "command",
    "embed", "hr", "img", "input", "keygen", "link", "meta",
    "param", "source", "track", "wbr"
  )

  def isSelfClosing(tag: String): Boolean = {
    selfClosingTags.contains(tag.toLowerCase)
  }
}
