package parser

import model.Sound

object SoundBlock extends Block[Sound] {
  object Header extends Block.Header("Sound")

  override def parseHeader: PartialFunction[String, Sound] = {
    case Header(id) => Sound(
      id = id
    )
  }

  override def parseProperty(current: Sound): PartialFunction[String, Sound] =
    /* currently, we don't care about any Sound properties */
    PartialFunction.empty
}
