import scala.io.Source

// TODO:
//  - Filters should ideally recurse through var1 and var2 references to other objects
//  - Currently, we just replace var1 and var2 value with the generated name if we are
//    in port mode. This isn't correct in cases where a variable has upper bits used
//    for something else (e.g. A_SpawnObjectRelative's var2 has lower 16 bits for object
//    name).
//  - We should only port var1 and var2 if we upgraded their targets in this filter, possibly.

object SocTool extends App {

  def error(message: String) = {
    throw new Exception(s"** Error: $message")
  }

  def argValue(argNames: Set[String]): Option[String] =
    args.dropWhile(arg => !argNames.contains(arg)).drop(1).headOption

  def argFlag(argNames: Set[String]): Boolean =
    args.dropWhile(arg => !argNames.contains(arg)).nonEmpty

  def toIdList(arg: Option[String]) = {
    arg.getOrElse("").split(',').filter(_.nonEmpty)
  }

  val things = toIdList(argValue(Set("--thing-ids")))
  val states = toIdList(argValue(Set("--state-ids")))
  val sounds = toIdList(argValue(Set("--sound-ids")))
  val levels = toIdList(argValue(Set("--level-ids")))

  val action = argValue(Set("--action", "-a"))
  val socFile = argValue(Set("--soc", "-s"))

  val fromOld = argFlag(Set("--from-old-srb2"))
  val toLua = argFlag(Set("--to-lua", "-l"))
  val portable = argFlag(Set("--portable", "-p"))
  val genFreeSlots = argFlag(Set("--freeslots", "-f"))

  def loadFile(): Option[Source] = {
    val file = socFile.map(fileName => Source.fromFile(fileName))
    file
  }

  def doExtract(): Unit = {
    val extracted = loadFile() match {
      case Some(file) =>
        val lines = file.getLines()
        val script = SocScript(lines.toSeq)

        if (Seq(things, states, sounds, levels).forall(_.isEmpty))
          // Return all if no filters.
          script
        else {
          val withThings = things.foldLeft(SocScript())((soc, id) => script.extractThing(id, soc))
          val withStates = states.foldLeft(withThings)((soc, id) => script.extractState(id, soc))
          val withSounds = sounds.foldLeft(withStates)((soc, id) => script.extractSound(id, soc))
          val withLevels = levels.foldLeft(withSounds)((soc, id) => script.extractLevel(id, soc))

          withLevels
        }
      case None => error("SOC file not found")
    }

    // TODO: there are requirements for these names for length
    val ported = if (portable) MakePortable(
      SlotRenameRules(
        thingId = id => Some(s"MT_$id"),
        stateId = id => Some(s"S_$id"),
        soundId = id => Some(s"sfx_$id"),
        spriteId = id => {
          val newId = "0" * (4 - id.length) + id

          if (newId.length > 4)
            error("Currently, generating SFX names for IDs longer than 4 digits is not supported.")

          Some(s"SPR_$newId")
        }
      )
    )(extracted) else extracted

    val genSlots = if (genFreeSlots) GenerateFreeSlots(ported) else ported

    val print: SocScript => Unit = if (toLua)
      PrintAsLua(PrinterConfig())
    else
      PrintAsSoc(PrinterConfig())

    /* print extracted blocks to stdout */
    print(genSlots)
  }

  action.map(_.toLowerCase) match {
    case Some("extract") => doExtract()
    case None => error("missing entity type")
  }
}
