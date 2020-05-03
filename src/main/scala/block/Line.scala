package block

import scala.util.Try

// Snippet of interesting compiler bug that works. Saving here for now.
//object Value {
//  def unapply[T](kv: (String, T)): Option[T] = Some(kv._2)
//}
//
//override def parseProperty(level: Level): PartialFunction[String, Level] = {
//  case NextLevelLine(Value(levelId)) => level.copy(nextLevel = Some(levelId))
//}

object Line {
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

  trait WellDefinedKey {
    val keyName: String
  }

  trait KeyExactly extends KeyFilter with WellDefinedKey {
    val ignoreCase: Boolean = true
    override val keyName: String

    //override val writeKey: String = keyName
    override def keyFilter(key: String): Boolean =
      if (ignoreCase) key.equalsIgnoreCase(keyName) else key.equals(keyName)
  }

  trait KeyIn extends KeyFilter {
    val keys: Set[String]
    override def keyFilter(key: String): Boolean =
      keys.map(s => s.toUpperCase()).contains(key.toUpperCase())
  }

  trait LineValueParser[T] extends (String => Option[T]) { }

  object LineValueParser {
    implicit object StringParser extends LineValueParser[String] {
      override def apply(str: String): Option[String] = Some(str)
    }

    implicit object IntParser extends LineValueParser[Int] {
      override def apply(str: String): Option[Int] = Try(str.toInt).toOption
    }
  }

  class Like[T]()(implicit converter: LineValueParser[T]) {
    this: Delimiter with KeyFilter =>

    def unapply(line: String): Option[T] = {
      val nonComment = line.split(Block.CommentSignifier).headOption.getOrElse("")
      val parts = nonComment.split(delimiter)

      val keyValue = (parts.headOption, parts.tail.headOption)
      keyValue match {
        case (Some(key), Some(value)) if keyFilter(key) => converter(value)
        case _ => None
      }
    }
  }

  class Distinct[T](implicit converter: LineValueParser[T]) extends Like[T]()(converter) {
    this: Delimiter with KeyFilter with WellDefinedKey =>

    def writeValueToString(value: T): String = value.toString

    def apply(value: T): String = {
      s"$keyName$delimiter$value"
    }
  }
}
