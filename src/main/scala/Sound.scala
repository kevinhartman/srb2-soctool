case class Sound(
  length: Int
)

object Sound extends Block[Sound] {
  object Header extends Block.Header("Sound")

  override def initialState(contents: Seq[String]): Sound = Sound(
    length = contents.length
  )

  override def parseProperty(current: Sound): PartialFunction[String, Sound] =
    /* currently, we don't care about any Sound properties */
    PartialFunction.empty
}
