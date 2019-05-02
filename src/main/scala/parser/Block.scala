package parser

trait Block[T] {
  def readEntity(initialState: T, body: Seq[String]): T = {
    body.foldLeft(initialState)((current, line) =>
      parseProperty(current).applyOrElse[String, T](line, _ => {
        println(s"Warning: couldn't parse line: $line")
        current
      })
    )
  }

  def unapply(lines: Seq[String]): Option[T] = {
    val header = lines.headOption
    header.collect {
      parseHeader.andThen(state => {
        readEntity(state, lines.tail)
      })
    }
  }

  def parseHeader: PartialFunction[String, T]
  def parseProperty(current: T): PartialFunction[String, T]
}

object Block {
  import PropertyLine._

  class Header(val entityType: String) extends ValueOf(new HeaderProp(entityType))
  class HeaderProp(val entityType: String) extends PropertyLine[Int]
    with SpaceDelimiter
    with KeyExactly
  {
    override val keyName: String = entityType
  }
}
