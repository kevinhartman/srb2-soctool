package block

import model.State

object StateBlock extends Block[State] {
  import Line.{KeyExactly, EqualsDelimiter, SpaceDelimiter, KeyIn, WellDefinedKey}

  object HeaderLine extends Line.Distinct[Int]
    with SpaceDelimiter
    with KeyIn
    with WellDefinedKey
  {
    override val keyName: String = "FRAME"
    override val keys: Set[String] = Set("Frame", "FRAME", "State", "STATE")
  }

  object NextLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "NEXT"
  }

  object ActionLine extends Line.Distinct[String]
    with SpaceDelimiter
    with KeyExactly
  {
    override val keyName: String = "ACTION"
  }

  object SpriteNumberLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "SPRITENUMBER"
  }

  object SpriteSubNumberLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "SPRITESUBNUMBER"
  }

  object DurationLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "DURATION"
  }

  object Var1Line extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "VAR1"
  }

  object Var2Line extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "VAR2"
  }

  override def parseHeader: PartialFunction[String, State] = {
    case HeaderLine(id) => State(
      id = id
    )
  }

  override def parseProperty(entry: State): PartialFunction[String, State] = {
    case SpriteNumberLine(spriteNumber) => entry.copy(spriteNumber = Some(spriteNumber))
    case SpriteSubNumberLine(spriteSubNumber) => entry.copy(spriteSubNumber = Some(spriteSubNumber))
    case DurationLine(duration) => entry.copy(duration = Some(duration))
    case NextLine(nextStateId) => entry.copy(next = Some(nextStateId))
    case ActionLine(name) => entry.copy(action = Some(name))
    case Var1Line(var1) => entry.copy(var1 = Some(var1))
    case Var2Line(var2) => entry.copy(var2 = Some(var2))
  }
}