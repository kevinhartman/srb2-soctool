package parser

import model.State

object StateBlock extends Block[State] {
  import Line._

  object Header extends ValueOf(HeaderLine)
  object HeaderLine extends Line[Int]
    with SpaceDelimiter
    with KeyIn
  {
    override val keys: Set[String] = Set("Frame", "FRAME", "State", "STATE")
  }

  object Next extends ValueOf(NextLine)
  object NextLine extends Line[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "NEXT"
  }

  object Action extends ValueOf(ActionLine)
  object ActionLine extends Line[String]
    with SpaceDelimiter
    with KeyExactly
  {
    override val keyName: String = "ACTION"
  }

  override def parseHeader: PartialFunction[String, State] = {
    case Header(id) => State(
      id = id
    )
  }

  override def parseProperty(entry: State): PartialFunction[String, State] = {
    case Next(nextStateId) => entry.copy(next = Some(nextStateId))
    case Action(name) => entry.copy(action = Some(name))
  }
}