package model

case class Level(
  length: Int,
  nextLevel: Option[Int] = None
)

object Level extends Block[Level] {
  object Header extends Block.Header("Level")

  object NextLevel extends Property.ValueOf(new Property.Property[Int]
    with Property.EqualsDelimiter
    with Property.KeyExactly
  {
    override val keyName: String = "NEXTLEVEL"
  })

  override def initialState(contents: Seq[String]): Level = Level(
    length = contents.length
  )

  override def parseProperty(level: Level): PartialFunction[String, Level] = {
    case NextLevel(levelId) => level.copy(nextLevel = Some(levelId))
  }
}