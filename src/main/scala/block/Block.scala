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

  def apply(entity: T): Seq[String] = {
    Seq(writeHeader(entity)) ++ writeProperties(entity)
  }

  def writeHeader(entity: T): String
  def writeProperties(entity: T): Seq[String]
}

object Block {
  import Line._

  val CommentSignifier = "#"

  class HeaderLine(val entityType: String) extends Distinct[String]
    with SpaceDelimiter
    with KeyExactly
  {
    override val keyName: String = entityType
  }
}

