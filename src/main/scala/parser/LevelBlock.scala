package parser

import model.Level

object LevelBlock extends Block[Level] {
  object Header extends Block.Header("Level")

  object NextLevel extends Line.ValueOf(NextLevelLine)
  object NextLevelLine extends Line[Int]
    with Line.EqualsDelimiter
    with Line.KeyExactly
  {
    override val keyName: String = "NEXTLEVEL"
  }

  override def parseHeader: PartialFunction[String, Level] = {
    case Header(id) => Level(
      id = id
    )
  }

  override def parseProperty(level: Level): PartialFunction[String, Level] = {
    case NextLevel(levelId) => level.copy(nextLevel = Some(levelId))
  }
}
