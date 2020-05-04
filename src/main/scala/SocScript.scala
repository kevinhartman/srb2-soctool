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
  objects: Map[String, Entry[Object]] = Map(),
  states: Map[String, Entry[State]] = Map(),
  sounds: Map[String, Entry[Sound]] = Map(),

  dependencies: Dependencies = Dependencies()
) {
  def withFreeSlot(slot: String): SocScript = {
    this.copy(freeSlots = this.freeSlots + slot)
  }

  def withLevel(entityId: String, level: Entry[Level]): SocScript =
    this.copy(levels = this.levels + (entityId -> level))

  def withObject(entityId: String, obj: Entry[Object]): SocScript =
    this.copy(objects = this.objects + (entityId -> obj))

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

  def extractObject(id: String, result: SocScript = SocScript()): SocScript = {
    objects.get(id) match {
      case Some(objEntry) if !result.objects.contains(id) =>
        val withObject: SocScript = result.withObject(id, objEntry)

        val withStates = objEntry.entity.states.foldLeft(withObject)((res, stateId) =>
          extractState(stateId, res)
        )

        val withSounds = objEntry.entity.sounds.foldLeft(withStates)((res, soundId) =>
          extractSound(soundId, res)
        )

        withSounds
      case Some(_) => result // already processed
      case None =>
        if (id != "0") System.err.println(s"* Warning: Object $id not found in script.")
        result
    }
  }

  def extractState(id: String, result: SocScript = SocScript()): SocScript = {
    states.get(id) match {
      case Some(stateEntry) if !result.states.contains(id) =>
        val withState = result.withState(id, stateEntry)
        val withNextState = stateEntry.entity.next.map(extractState(_, withState)).getOrElse(withState)

        val withVar1Obj = stateEntry.entity.Var1AsObject().map(extractObject(_, withNextState)).getOrElse(withNextState)
        val withVar2Obj = stateEntry.entity.Var2AsObject().map(extractObject(_, withVar1Obj)).getOrElse(withVar1Obj)

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
  def readScript(indexedLines: BufferedIterator[(String, Int)]): SocScript = {
    def isComment(line: String) = line.startsWith(Block.CommentSignifier)
    def isBlank(line: String) = line.trim.isEmpty

    def readBlock(itr: Iterator[(String, Int)]): (Seq[String], BufferedIterator[(String, Int)]) = {
      // we need to dup the iterator since we want to read/manipulate block vs rest
      // differently.
      val (blockItr, restItr) = itr.duplicate

      val block = blockItr.map(_._1).takeWhile(!isBlank(_)).toSeq
      val unread = restItr.drop(block.length)

      (block, unread.buffered)
    }

    def readLine(itr: Iterator[(String, Int)]): (Option[String], BufferedIterator[(String, Int)]) = {
      val bufferedItr = itr.buffered
      val line = bufferedItr.headOption.map(_._1)

      (line, bufferedItr.drop(1).buffered)
    }

    // save the current offset before reading (it's safe to call headOption
    // and then continue to use indexedLines)
    val offset = indexedLines.headOption.map(_._2)

    val (_tempItr1, _tempItr2) = indexedLines.duplicate
    val (line, afterLineItr) = readLine(_tempItr1)
    val (block, afterBlockItr) = readBlock(_tempItr2)

    def entry[T](entity: T) = Entry(offset.get, block.length, List(), entity)

    block match {
      case Seq(comment, _*) if isComment(comment) => {
        readScript(afterLineItr) /* skip comment */
      }
      case LevelBlock(level) => readScript(afterBlockItr).withLevel(level.id, entry(level))
      case ObjectBlock(obj) => readScript(afterBlockItr).withObject(obj.id, entry(obj))
      case StateBlock(state) => readScript(afterBlockItr).withState(state.id, entry(state))
      case SoundBlock(sound) => readScript(afterBlockItr).withSound(sound.id, entry(sound))
      case Seq("Freeslot", freeslots @ _*) => {
        val next = readScript(afterBlockItr)
        next.copy(freeSlots = next.freeSlots ++ freeslots)
      }
      case emptyBlock if emptyBlock.isEmpty =>
        if (line.isDefined) {
          /* skip blank line */
          readScript(afterLineItr)
        }
        else
          /* end of script: return empty script */
          SocScript()
      // case Seq(comment) if isComment(comment)=> // TODO: test that this line isn't needed
      case _ =>
        System.err.println(s"Failed to parse block: $block")
        throw new Exception("Unknown block.")
    }
  }

  def apply(lines: Iterator[String]): SocScript = readScript(lines.zipWithIndex.buffered)
}
