package parser

import model.Thing

object ThingBlock extends Block[Thing] {
  import PropertyLine._

  object Header extends Block.Header("Thing")
  object StateDependency extends PropertyLine[Int]
    with EqualsDelimiter
    with KeyFilter
  {
    override def keyFilter(key: String): Boolean = key.toUpperCase.endsWith("STATE")
  }

  object SoundDependency extends PropertyLine[Int]
    with EqualsDelimiter
    with KeyFilter
  {
    override def keyFilter(key: String): Boolean = key.toUpperCase.endsWith("SOUND")
  }

  override def parseHeader: PartialFunction[String, Thing] = {
    case Header(id) => Thing(
      id = id
    )
  }

  override def parseProperty(entry: Thing): PartialFunction[String, Thing] = {
    case StateDependency(property, id) => entry.copy(states = entry.states + (property -> id))
    case SoundDependency(property, id) => entry.copy(sounds = entry.sounds + (property -> id))
  }
//
//  override def parseProperty(entry: Thing): PartialFunction[String, Thing] = {
//    case StateDependency(property, id) =>
//      entry.copy(states = entry.states + (property -> id))
//      parseState(entry.copy(states = entry.states + (property -> id)), )
//    case SoundDependency(property, id) => entry.copy(sounds = entry.sounds + (property -> id))
//  }
//
//  private def parseState(entry: Thing, line: String): Thing = (_ => entry)
}
