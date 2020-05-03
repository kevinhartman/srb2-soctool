package block

import block.Line.{KeyIn, WellDefinedKey}
import model.Level

object LevelBlock extends Block[Level] {
  import Line.{KeyExactly, EqualsDelimiter}
  object HeaderLine extends Block.HeaderLine("Level")

  object LevelNameLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "Levelname"
  }

  object ActLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "Act"
  }

  object NextLevelLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "NextLevel"
  }

  object MusicSlotLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyIn
    with WellDefinedKey
  {
    override val keyName: String = "Music"
    override val keys: Set[String] = Set("MusicSlot", "Music")
  }

  object WeatherLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "WEATHER"
  }

  object SkyNumLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "SkyNum"
  }

  object SkyboxScaleLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "SkyboxScale"
  }

  object RecordAttackLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "RecordAttack"
  }

  object NoZoneLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "NOZONE"
  }

  object NoSSMusicLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "NOSSMUSIC"
  }

  object LevelSelectLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "LevelSelect"
  }

  object TypeOfLevelLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "TypeOfLevel"
  }

  object SaveGameLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "SaveGame"
  }

  object FlickyListLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "FlickyList"
  }

  override def parseHeader: PartialFunction[String, Level] = {
    case HeaderLine(id) => Level(
      id = id
    )
  }

  override def parseProperty(level: Level): PartialFunction[String, Level] = {
    case LevelNameLine(name)          => level.copy(levelName = Some(name))
    case ActLine(act)                 => level.copy(act = Some(act))
    case NextLevelLine(levelId)       => level.copy(nextLevel = Some(levelId))
    case MusicSlotLine(slot)          => level.copy(musicSlot = Some(slot))
    case WeatherLine(weather)         => level.copy(weather = Some(weather))
    case SkyNumLine(num)              => level.copy(skyNum = Some(num))
    case SkyboxScaleLine(scale)       => level.copy(skyboxScale = Some(scale))
    case RecordAttackLine(rec)        => level.copy(recordAttack = Some(rec))
    case SaveGameLine(save)           => level.copy(saveGame = Some(save))
    case NoZoneLine(noZone)           => level.copy(noZone = Some(noZone))
    case NoSSMusicLine(noSSMusic)     => level.copy(noSSMusic = Some(noSSMusic))
    case LevelSelectLine(levelSelect) => level.copy(levelSelect = Some(levelSelect))
    case TypeOfLevelLine(typeOfLevel) => level.copy(typeOfLevel = Some(typeOfLevel))
    case FlickyListLine(flickies)     => level.copy(flickyList = Some(flickies))
  }

  override def writeHeader(level: Level): String = HeaderLine(level.id)

  override def writeProperties(level: Level): Seq[String] = {
    Seq(
      level.levelName   .map(LevelNameLine(_)),
      level.act         .map(ActLine(_)),
      level.nextLevel   .map(NextLevelLine(_)),
      level.musicSlot   .map(MusicSlotLine(_)),
      level.weather     .map(WeatherLine(_)),
      level.skyNum      .map(SkyNumLine(_)),
      level.skyboxScale .map(SkyboxScaleLine(_)),
      level.recordAttack.map(RecordAttackLine(_)),
      level.saveGame    .map(SaveGameLine(_)),
      level.noZone      .map(NoZoneLine(_)),
      level.noSSMusic   .map(NoSSMusicLine(_)),
      level.levelSelect .map(LevelSelectLine(_)),
      level.typeOfLevel .map(TypeOfLevelLine(_)),
      level.flickyList  .map(FlickyListLine(_))
    ).flatten
  }
}
