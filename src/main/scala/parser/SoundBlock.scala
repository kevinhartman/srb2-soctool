package parser

import model.Sound

object SoundBlock extends Block[Sound] {
  import Line.{KeyExactly, EqualsDelimiter}
  object HeaderLine extends Block.HeaderLine("SOUND")

  object PriorityLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "PRIORITY"
  }

  object FlagsLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "FLAGS"
  }

  override def parseHeader: PartialFunction[String, Sound] = {
    case HeaderLine(id) => Sound(
      id = id
    )
  }

  override def parseProperty(sound: Sound): PartialFunction[String, Sound] = {
    case PriorityLine(priority) => sound.copy(priority = Some(priority))
    case FlagsLine(flags) => sound.copy(flags = Some(flags))
  }
}
