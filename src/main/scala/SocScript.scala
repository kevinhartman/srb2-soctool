case class SocScript(
  levels: Map[Int, Level] = Map(),
  things: Map[Int, Thing] = Map(),
  states: Map[Int, State] = Map(),
  sounds: Map[Int, Sound] = Map()
) {
  def withLevel(entityId: Int, level: Level): SocScript =
    this.copy(levels = this.levels + (entityId -> level))

  def withThing(entityId: Int, thing: Thing): SocScript =
    this.copy(things = this.things + (entityId -> thing))

  def withState(entityId: Int, state: State): SocScript =
    this.copy(states = this.states + (entityId -> state))

  def withSound(entityId: Int, sound: Sound): SocScript =
    this.copy(sounds = this.sounds + (entityId -> sound))
}

object SocScript {
  def readScript(lines: Seq[String]): SocScript = {
    lines.headOption match {
      case Some(Level.Header(entityId)) =>
        val level = Level(lines)
        val unread = lines.drop(level.length)
        readScript(unread).withLevel(entityId, level)

      case Some(Thing.Header(entityId)) =>
        val thing = Thing(lines)
        val unread = lines.drop(thing.length)
        readScript(unread).withThing(entityId, thing)

      case Some(State.Header(entityId)) =>
        val state = State(lines)
        val unread = lines.drop(state.length)
        readScript(unread).withState(entityId, state)

      case Some(Sound.Header(entityId)) =>
        val sound = Sound(lines)
        val unread = lines.drop(sound.length)
        readScript(unread).withSound(entityId, sound)

      case Some(line) =>
        /* skip if empty line or comment */
        val validNonmatch = Block.isBlank(line) || Block.isComment(line)
        if (!validNonmatch) {
          print(s"Failed to parse header: $line")
          assert(false)
        }

        readScript(lines.drop(1))
      case None =>
        /* end of script: return empty script */
        SocScript()
    }
  }

  def apply(lines: Seq[String]): SocScript = readScript(lines)
}
