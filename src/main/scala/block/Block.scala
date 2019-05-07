package block

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
  import Line._

  class HeaderLine(val entityType: String) extends Distinct[Int]
    with SpaceDelimiter
    with KeyExactly
  {
    override val keyName: String = entityType
  }
}

