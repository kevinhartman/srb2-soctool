package block

import model.Level

object LevelBlock extends Block[Level] {
  import Line.{KeyExactly, EqualsDelimiter}
  object HeaderLine extends Block.HeaderLine("Level")

  object LevelNameLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "LEVELNAME"
  }

  object NextLevelLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "NEXTLEVEL"
  }

  object MusicSlotLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "MUSICSLOT"
  }

  object WeatherLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "WEATHER"
  }

  object SkyNumLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "SKYNUM"
  }

  object NoZoneLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "NOZONE"
  }

  object NoSSMusicLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "NOSSMUSIC"
  }

  object LevelSelectLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "LEVELSELECT"
  }

  object TypeOfLevelLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "TYPEOFLEVEL"
  }

  override def parseHeader: PartialFunction[String, Level] = {
    case HeaderLine(id) => Level(
      id = id
    )
  }

  override def parseProperty(level: Level): PartialFunction[String, Level] = {
    case LevelNameLine(name) => level.copy(levelName = Some(name))
    case NextLevelLine(levelId) => level.copy(nextLevel = Some(levelId))
    case MusicSlotLine(slot) => level.copy(musicSlot = Some(slot))
    case WeatherLine(weather) => level.copy(weather = Some(weather))
    case SkyNumLine(num) => level.copy(skyNum = Some(num))
    case NoZoneLine(noZone) => level.copy(noZone = Some(noZone))
    case NoSSMusicLine(noSSMusic) => level.copy(noSSMusic = Some(noSSMusic))
    case LevelSelectLine(levelSelect) => level.copy(levelSelect = Some(levelSelect))
    case TypeOfLevelLine(typeOfLevel) => level.copy(typeOfLevel = Some(typeOfLevel))
  }

  def test() = {
    NoZoneLine.apply(4)
  }
}
