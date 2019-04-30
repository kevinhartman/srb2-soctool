package model

case class State(
  length: Int,
  next: Option[Int],
  action: Option[String]
)

object State extends Block[State] {
  import Property._

  object Header extends ValueOf(BlockHeaderProp)
  object BlockHeaderProp extends Property[Int]
    with SpaceDelimiter
    with KeyIn
  {
    override val keys: Set[String] = Set("Frame", "FRAME", "State", "STATE")
  }

  object Next extends ValueOf(NextProp)
  object NextProp extends Property[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "NEXT"
  }

  object Action extends ValueOf(ActionProp)
  object ActionProp extends Property[String]
    with SpaceDelimiter
    with KeyExactly
  {
    override val keyName: String = "ACTION"
  }

  override def initialState(contents: Seq[String]): State = State(
    length = contents.size,
    next = None,
    action = None
  )

  override def parseProperty(entry: State): PartialFunction[String, State] = {
    case Next(nextStateId) => entry.copy(next = Some(nextStateId))
    case Action(name) => entry.copy(action = Some(name))
  }
}