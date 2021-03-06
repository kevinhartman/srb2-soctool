package block

import model.Sound

object SoundBlock extends Block[Sound] {
  import Line.{KeyExactly, EqualsDelimiter}
  object HeaderLine extends Block.HeaderLine("Sound")

  object SingularLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "Singular"
  }

  object PriorityLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "Priority"
  }

  object FlagsLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "Flags"
  }

  override def parseHeader: PartialFunction[String, Sound] = {
    case HeaderLine(id) => Sound(
      id = id
    )
  }

  override def parseProperty(sound: Sound): PartialFunction[String, Sound] = {
    case SingularLine(singular) => sound.copy(singular = Some(singular))
    case PriorityLine(priority) => sound.copy(priority = Some(priority))
    case FlagsLine(flags)       => sound.copy(flags = Some(flags))
  }

  override def writeHeader(sound: Sound): String = HeaderLine(sound.id)

  override def writeProperties(sound: Sound): Seq[String] = {
    Seq(
      sound.singular.map(SingularLine(_)),
      sound.priority.map(PriorityLine(_)),
      sound.flags   .map(FlagsLine(_))
    ).flatten
  }
}
