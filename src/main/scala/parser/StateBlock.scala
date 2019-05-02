package parser

import model.State

object StateBlock extends Block[State] {
  import PropertyLine._

  object Header extends ValueOf(BlockHeaderProp)
  object BlockHeaderProp extends PropertyLine[Int]
    with SpaceDelimiter
    with KeyIn
  {
    override val keys: Set[String] = Set("Frame", "FRAME", "State", "STATE")
  }

  object Next extends ValueOf(NextProp)
  object NextProp extends PropertyLine[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "NEXT"
  }

  object Action extends ValueOf(ActionProp)
  object ActionProp extends PropertyLine[String]
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