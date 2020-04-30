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

  val entityType = argValue(Set("--type", "-t"))
  val entityId = argValue(Set("--id", "-d"))
  val action = argValue(Set("--action", "-a"))
  val socFile = argValue(Set("--soc", "-s"))
  //val thingFreeslot = argValue(Set("--thing-freeslot"))

  val fromOld = argFlag(Set("--from-old-srb2"))
  val toLua = argFlag(Set("--to-lua", "-l"))
  val portable = argFlag(Set("--portable", "-p"))
  val genFreeSlots = argFlag(Set("--freeslots", "-f"))

  def loadFile(): Option[Source] = {
    val file = socFile.map(fileName => Source.fromFile(fileName))
    file
  }

  def doExtract(): Unit = {
    val entity = entityType.getOrElse(error("Missing entity type"))
    val id = entityId.getOrElse(error("Missing entity id"))

    val extracted = loadFile() match {
      case Some(file) =>
        val lines = file.getLines()
        val script = SocScript(lines.toSeq)

        entity.toUpperCase match {
          case "THING" => script.extractThing(id)
          case "LEVEL" => {
            if (toLua) error("Levels cannot be represented in Lua.")

            script.extractLevel(id)
          }
          case "STATE" => script.extractState(id)
          case "SOUND" => script.extractSound(id)
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
