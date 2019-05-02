package parser

import scala.util.Try

object PropertyLine {

  trait Delimiter {
    val delimiter: String
  }

  trait SpaceDelimiter extends Delimiter {
    override val delimiter: String = " "
  }

  trait EqualsDelimiter extends Delimiter {
    override val delimiter: String = " = "
  }

  trait KeyFilter {
    def keyFilter(key: String): Boolean
  }

  trait KeyExactly extends KeyFilter {
    val ignoreCase: Boolean = true
    val keyName: String
    override def keyFilter(key: String): Boolean =
      if (ignoreCase) key.equalsIgnoreCase(keyName) else key.equals(keyName)
  }

  trait KeyIn extends KeyFilter {
    val keys: Set[String]
    override def keyFilter(key: String): Boolean = keys.contains(key)
  }

  trait LineValueParser[T] extends (String => Option[T]) { }

  // TODO handle post-value comments
  object LineValueParser {
    implicit object StringParser extends LineValueParser[String] {
      override def apply(str: String): Option[String] = Some(str)
    }

    implicit object IntParser extends LineValueParser[Int] {
      override def apply(str: String): Option[Int] = Try(str.toInt).toOption
    }
  }

  class PropertyLine[T]()(implicit converter: LineValueParser[T]) {
    this: Delimiter with KeyFilter =>

    def unapply(line: String): Option[(String, T)] = {
      val parts = line.split(delimiter)

      val keyValue = (parts.headOption, parts.tail.headOption)
      keyValue match {
        case (Some(key), Some(value)) if keyFilter(key) => converter(value).map((key, _))
        case _ => None
      }
    }
  }

  class ValueOf[T](val property: PropertyLine[T]) {
    def unapply(line: String): Option[T] = property.unapply(line).map(_._2)
  }
}
