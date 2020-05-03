import model._
import block._

case class Entry[T](
  offset: Int,
  length: Int,
  warnings: List[String],
  entity: T
)

case class Dependencies(
  externStates: Set[String] = Set(),
  externObjects: Set[String] = Set(),
  externSprites: Set[String] = Set(),
  externSounds: Set[String] = Set(),
  spriteFiles: Set[String] = Set(),
  soundsFiles: Set[String] = Set(),
  lineDefs: Set[String] = Set()
)

case class SocScript(
  freeSlots: Set[String] = Set(),
  levels: Map[String, Entry[Level]] = Map(),
  things: Map[String, Entry[Thing]] = Map(),
  states: Map[String, Entry[State]] = Map(),
  sounds: Map[String, Entry[Sound]] = Map(),

  dependencies: Dependencies = Dependencies()
) {
  def withFreeSlot(slot: String): SocScript = {
    this.copy(freeSlots = this.freeSlots + slot)
  }

  def withLevel(entityId: String, level: Entry[Level]): SocScript =
    this.copy(levels = this.levels + (entityId -> level))

  def withThing(entityId: String, thing: Entry[Thing]): SocScript =
    this.copy(things = this.things + (entityId -> thing))

  def withState(entityId: String, state: Entry[State]): SocScript =
    this.copy(states = this.states + (entityId -> state))

  def withSound(entityId: String, sound: Entry[Sound]): SocScript =
    this.copy(sounds = this.sounds + (entityId -> sound))

  // TODO: when an entity is not found, it'd be possible to include it in a missing
  //       reference list of some sort.

  def extractLevel(id: String, result: SocScript = SocScript()): SocScript = {
    levels.get(id) match {
      case Some(levelEntry) if !result.levels.contains(id) =>
        result.withLevel(id, levelEntry)
      case Some(_) => result // already processed
      case None =>
        if (id != "0") System.err.println(s"* Warning: Level $id not found in script.")
        result
    }
  }

  def extractThing(id: String, result: SocScript = SocScript()): SocScript = {
    things.get(id) match {
      case Some(thingEntry) if !result.things.contains(id) =>
        val withThing: SocScript = result.withThing(id, thingEntry)

        val withStates = thingEntry.entity.states.foldLeft(withThing)((res, stateId) =>
          extractState(stateId, res)
        )

        val withSounds = thingEntry.entity.sounds.foldLeft(withStates)((res, soundId) =>
          extractSound(soundId, res)
        )

        withSounds
      case Some(_) => result // already processed
      case None =>
        if (id != "0") System.err.println(s"* Warning: Thing $id not found in script.")
        result
    }
  }

  def extractState(id: String, result: SocScript = SocScript()): SocScript = {
    states.get(id) match {
      case Some(stateEntry) if !result.states.contains(id) =>
        val withState = result.withState(id, stateEntry)
        val withNextState = stateEntry.entity.next.map(extractState(_, withState)).getOrElse(withState)

        val withVar1Obj = stateEntry.entity.Var1AsThing().map(extractThing(_, withNextState)).getOrElse(withNextState)
        val withVar2Obj = stateEntry.entity.Var2AsThing().map(extractThing(_, withVar1Obj)).getOrElse(withVar1Obj)

        withVar2Obj
      case Some(_) => result // already processed
      case _ =>
        if (id != "0") System.err.println(s"* Warning: State $id not found in script.")
        result
    }
  }

  def extractSound(id: String, result: SocScript = SocScript()): SocScript = {
    sounds.get(id) match {
      case Some(soundEntry) if !result.sounds.contains(id) =>
        result.withSound(id, soundEntry)
      case Some(_) => result // already processed
      case _ =>
        if (id != "0") System.err.println(s"* Warning: Sound $id not found in script.")
        result
    }
  }
}

object SocScript {
  def readScript(indexedLines: Seq[(String, Int)]): SocScript = {
    def isComment(line: String) = line.startsWith(Block.CommentSignifier)
    def isBlank(line: String) = line.trim.isEmpty

    val lines = indexedLines.map(_._1)
    val block = lines.takeWhile(!isBlank(_))

    val unread = indexedLines.drop(block.length)
    def nextBlock = readScript(unread)

    val offset = indexedLines.headOption.map(_._2)
    def entry[T](entity: T) = Entry(offset.get, block.length, List(), entity)

    block match {
      case Seq(comment, _*) if isComment(comment) => readScript(indexedLines.drop(1)) /* skip comment */
      case LevelBlock(level) => nextBlock.withLevel(level.id, entry(level))
      case ThingBlock(thing) => nextBlock.withThing(thing.id, entry(thing))
      case StateBlock(state) => nextBlock.withState(state.id, entry(state))
      case SoundBlock(sound) => nextBlock.withSound(sound.id, entry(sound))
      case Seq("Freeslot", freeslots @ _*) => nextBlock.copy(freeSlots = nextBlock.freeSlots ++ freeslots)
      case emptyBlock if emptyBlock.isEmpty =>
        if (indexedLines.nonEmpty)
          /* skip blank line */
          readScript(indexedLines.drop(1))
        else
          /* end of script: return empty script */
          SocScript()
      // case Seq(comment) if isComment(comment)=> // TODO: test that this line isn't needed
      case _ =>
        System.err.println(s"Failed to parse block: $block")
        throw new Exception("Unknown block.")
    }
  }

  def apply(lines: Seq[String]): SocScript = readScript(lines.zipWithIndex)
}
