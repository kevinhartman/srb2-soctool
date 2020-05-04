package block

import model.State

object StateBlock extends Block[State] {
  import Line.{KeyExactly, EqualsDelimiter, SpaceDelimiter, KeyIn, WellDefinedKey}

  object HeaderLine extends Line.Distinct[String]
    with SpaceDelimiter
    with KeyIn
    with WellDefinedKey
  {
    override val keyName: String = "State"
    override val keys: Set[String] = Set("Frame", "State")
  }

  object NextLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "Next"
  }

  object ActionLine extends Line.Distinct[String]
    with SpaceDelimiter
    with KeyExactly
  {
    override val keyName: String = "Action"
  }

  object SpriteNumberLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyIn
    with WellDefinedKey
  {
    override val keyName: String = "SpriteName"
    override val keys: Set[String] = Set("SpriteName", "SpriteNumber")
  }

  object SpriteSubNumberLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyIn
    with WellDefinedKey
  {
    override val keyName: String = "SpriteFrame"
    override val keys: Set[String] = Set("SpriteFrame", "SpriteSubNumber")
  }

  object DurationLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "Duration"
  }

  object Var1Line extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "Var1"
  }

  object Var2Line extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "Var2"
  }

  override def parseHeader: PartialFunction[String, State] = {
    case HeaderLine(id) => State(
      id = id
    )
  }

  override def parseProperty(entry: State): PartialFunction[String, State] = {
    case SpriteNumberLine(spriteNumber)       => entry.copy(spriteNumber = Some(spriteNumber))
    case SpriteSubNumberLine(spriteSubNumber) => entry.copy(spriteSubNumber = Some(spriteSubNumber))
    case DurationLine(duration)               => entry.copy(duration = Some(duration))
    case NextLine(nextStateId)                => entry.copy(next = Some(nextStateId))
    case ActionLine(name)                     => entry.copy(action = Some(name))
    case Var1Line(var1)                       => entry.copy(var1 = Some(var1))
    case Var2Line(var2)                       => entry.copy(var2 = Some(var2))
  }

  override def writeHeader(state: State): String = HeaderLine(state.id)

  override def writeProperties(state: State): Seq[String] = {
    Seq(
      state.spriteNumber   .map(SpriteNumberLine(_)),
      state.spriteSubNumber.map(SpriteSubNumberLine(_)),
      state.duration       .map(DurationLine(_)),
      state.next           .map(NextLine(_)),
      state.action         .map(ActionLine(_)),
      state.var1           .map(Var1Line(_)),
      state.var2           .map(Var2Line(_))
    ).flatten
  }
}