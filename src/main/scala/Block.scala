trait Block[T] {
  def apply(lines: Seq[String]): T = {
    val header = lines.headOption
    val body = lines.tail.takeWhile(!Block.isBlank(_))

    val init = initialState(header.toSeq ++ body)
    body.foldLeft(init)((current, line) =>
      parseProperty(current).applyOrElse[String, T](line, _ => {
        println(s"Warning: couldn't parse line: $line")
        current
      })
    )
  }

  def initialState(contents: Seq[String]): T
  def parseProperty(current: T): PartialFunction[String, T]
}

object Block {
  import Property._

  class Header(val entityType: String) extends ValueOf(new HeaderProp(entityType))
  class HeaderProp(val entityType: String) extends Property.Property[Int]
    with SpaceDelimiter
    with KeyExactly
  {
    override val keyName: String = entityType
  }

  def isComment(line: String): Boolean = line.startsWith("#")
  def isBlank(line: String): Boolean = line.trim.isEmpty
}
