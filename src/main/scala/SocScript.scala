case class Entry[T](
  offset: Int,
  entity: T
)

case class SocScript(
  levels: Map[Int, Entry[Level]] = Map(),
  things: Map[Int, Entry[Thing]] = Map(),
  states: Map[Int, Entry[State]] = Map(),
  sounds: Map[Int, Entry[Sound]] = Map()
) {
  def withLevel(entityId: Int, level: Entry[Level]): SocScript =
    this.copy(levels = this.levels + (entityId -> level))

  def withThing(entityId: Int, thing: Entry[Thing]): SocScript =
    this.copy(things = this.things + (entityId -> thing))

  def withState(entityId: Int, state: Entry[State]): SocScript =
    this.copy(states = this.states + (entityId -> state))

  def withSound(entityId: Int, sound: Entry[Sound]): SocScript =
    this.copy(sounds = this.sounds + (entityId -> sound))
}

object SocScript {
  def readScript(indexedLines: Seq[(String, Int)]): SocScript = {
    val lines = indexedLines.map(_._1)
    val offset = indexedLines.headOption.map(_._2)

    lines.headOption match {
      case Some(Level.Header(entityId)) =>
        val level = Level(lines)
        val unread = indexedLines.drop(level.length)
        readScript(unread).withLevel(entityId, Entry(offset.get, level))

      case Some(Thing.Header(entityId)) =>
        val thing = Thing(lines)
        val unread = indexedLines.drop(thing.length)
        readScript(unread).withThing(entityId, Entry(offset.get, thing))

      case Some(State.Header(entityId)) =>
        val state = State(lines)
        val unread = indexedLines.drop(state.length)
        readScript(unread).withState(entityId, Entry(offset.get, state))

      case Some(Sound.Header(entityId)) =>
        val sound = Sound(lines)
        val unread = indexedLines.drop(sound.length)
        readScript(unread).withSound(entityId, Entry(offset.get, sound))

      case Some(line) =>
        /* skip if empty line or comment */
        val validNonmatch = Block.isBlank(line) || Block.isComment(line)
        if (!validNonmatch) {
          print(s"Failed to parse header: $line")
          assert(false)
        }

        readScript(indexedLines.drop(1))
      case None =>
        /* end of script: return empty script */
        SocScript()
    }
  }

  def apply(lines: Seq[String]): SocScript = readScript(lines.zipWithIndex)
}
