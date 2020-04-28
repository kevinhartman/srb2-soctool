import model._
import block._

case class Entry[T](
  offset: Int,
  length: Int,
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

  def extractLevel(id: Int, result: SocScript = SocScript()): SocScript = {
    levels.get(id) match {
      case Some(levelEntry) if !result.levels.contains(id) =>
        result.withLevel(id, levelEntry)
      case Some(_) => result // already processed
      case _ =>
        println(s"* Warning: Level $id not found in script.")
        result
    }
  }

  def extractThing(id: Int, result: SocScript = SocScript()): SocScript = {
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
        println(s"* Warning: Thing $id not found in script.")
        result
    }
  }

  def extractState(id: Int, result: SocScript = SocScript()): SocScript = {
    states.get(id) match {
      case Some(stateEntry) if !result.states.contains(id) =>
        val withState = result.withState(id, stateEntry)
        val withNextState = stateEntry.entity.next.map(extractState(_, withState))

        withNextState match {
          case Some(next) => next
          case None => withState
        }
      case Some(_) => result // already processed
      case _ =>
        println(s"* Warning: State $id not found in script.")
        result
    }
  }

  def extractSound(id: Int, result: SocScript = SocScript()): SocScript = {
    sounds.get(id) match {
      case Some(soundEntry) if !result.sounds.contains(id) =>
        result.withSound(id, soundEntry)
      case Some(_) => result // already processed
      case _ =>
        println(s"* Warning: Sound $id not found in script.")
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
    def entry[T](entity: T) = Entry(offset.get, block.length, entity)

    block match {
      case Seq(comment, _*) if isComment(comment) => readScript(indexedLines.drop(1)) /* skip comment */
      case LevelBlock(level) => nextBlock.withLevel(level.id, entry(level))
      case ThingBlock(thing) => nextBlock.withThing(thing.id, entry(thing))
      case StateBlock(state) => nextBlock.withState(state.id, entry(state))
      case SoundBlock(sound) => nextBlock.withSound(sound.id, entry(sound))
      case emptyBlock if emptyBlock.isEmpty =>
        if (indexedLines.nonEmpty)
          /* skip blank line */
          readScript(indexedLines.drop(1))
        else
          /* end of script: return empty script */
          SocScript()
      // case Seq(comment) if isComment(comment)=> // TODO: test that this line isn't needed
      case _ =>
          print(s"Failed to parse block: $block")
          throw new Exception("Unknown block.")
    }
  }

  def apply(lines: Seq[String]): SocScript = readScript(lines.zipWithIndex)
}
