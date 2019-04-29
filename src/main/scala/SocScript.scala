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
  private def isComment(line: String): Boolean = line.startsWith("#")
  private def isBlank(line: String): Boolean = line.trim.isEmpty

  def readLevel(lines: Seq[String]): Level = {
    /* TODO: stubbed */
    Level(0)
  }

  def readThing(lines: Seq[String]): Thing = {
    /* TODO: stubbed */
    Thing(0)
  }

  def readSound(lines: Seq[String]): Sound = {
    /* TODO: stubbed */
    Sound(0)
  }

  // TODO: tailrec?
  def readScript(lines: Seq[String]): SocScript = {
    lines.headOption match {
      case Some(Level.BlockHeader(entityId)) =>
        val level = readLevel(lines)
        val unread = lines.drop(level.length)
        readScript(unread).withLevel(entityId, level)

      case Some(Thing.BlockHeader(entityId)) =>
        val thing = readThing(lines)
        val unread = lines.drop(thing.length)
        readScript(unread).withThing(entityId, thing)

      case Some(State.BlockHeader(entityId)) =>
        val state = State(lines)
        val unread = lines.drop(state.length)
        readScript(unread).withState(entityId, state)

      case Some(Sound.BlockHeader(entityId)) =>
        val sound = readSound(lines)
        val unread = lines.drop(sound.length)
        readScript(unread).withSound(entityId, sound)

      case Some(line) =>
        /* skip if empty line or comment */
        assert(line.trim().isEmpty || isComment(line))
        readScript(lines.drop(1))
      case None =>
        /* end of script: return empty script */
        SocScript()
    }
  }

  def apply(lines: Seq[String]): SocScript = readScript(lines)
}
