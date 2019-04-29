import scala.language.implicitConversions
import scala.util.Try

trait Entry[T] {
  def apply(lines: Seq[String]): T = {
    val header = lines.headOption
    val body = lines.tail.takeWhile(!Entry.isBlank(_))

    val init = initialState(header.toSeq ++ body)
    body.foldLeft(init)(parse)
  }

  def initialState(contents: Seq[String]): T
  def parse(current: T, line: String): T
}

object Entry {
  class Header(val entityType: String) extends Property.SpaceDelimited[Int](entityType)(LineValueParser.IntParser)

  trait LineValueParser[T] extends (String => Option[T]) { }

  object LineValueParser {
    implicit object StringParser extends LineValueParser[String] {
      override def apply(str: String): Option[String] = Some(str)
    }

    implicit object IntParser extends LineValueParser[Int] {
      override def apply(str: String): Option[Int] = Try(str.toInt).toOption
    }
  }

  class Property[T](val key: String, delimiter: String = " = ")
    (implicit converter: LineValueParser[T]) {
    private val keyUpper = key.toUpperCase

    def unapply(line: String): Option[T] = {
      val parts = line.split(delimiter)

      val keyValue = (parts.headOption.map(_.toUpperCase), parts.tail.headOption)
      keyValue match {
        case (Some(`keyUpper`), Some(value)) => converter(value)
        case _ => None
      }
    }
  }

  object Property {
    class SpaceDelimited[T](key: String)(implicit lineValueParser: LineValueParser[T])
      extends Property[T](key, delimiter = " ")(lineValueParser)
  }

  def isComment(line: String): Boolean = line.startsWith("#")
  def isBlank(line: String): Boolean = line.trim.isEmpty
}
