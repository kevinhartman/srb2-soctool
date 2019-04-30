package model

case class Thing(
  length: Int,
  states: Map[String, Int] = Map(),
  sounds: Map[String, Int] = Map()
)

object Thing extends Block[Thing] {
  import Property._

  object Header extends Block.Header("Thing")
  object StateDependency extends Property[Int]
    with EqualsDelimiter
    with KeyFilter
  {
    override def keyFilter(key: String): Boolean = key.toUpperCase.endsWith("STATE")
  }

  object SoundDependency extends Property[Int]
    with EqualsDelimiter
    with KeyFilter
  {
    override def keyFilter(key: String): Boolean = key.toUpperCase.endsWith("SOUND")
  }

  override def initialState(contents: Seq[String]): Thing = Thing(
    length = contents.size
  )

  override def parseProperty(entry: Thing): PartialFunction[String, Thing] = {
    case StateDependency(property, id) => entry.copy(states = entry.states + (property -> id))
    case SoundDependency(property, id) => entry.copy(sounds = entry.sounds + (property -> id))
  }
}
