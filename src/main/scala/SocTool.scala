import scala.io.Source
import scala.util.{Success, Try}

// TODO:
//  - Add support to read Freeslot declarations from SOCs
//  - Filters should ideally recurse through var1 and var2 references to other objects
//  - Currently, we just replace var1 and var2 value with the generated name if we are
//    in port mode. This isn't correct in cases where a variable has upper bits used
//    for something else (e.g. A_SpawnObjectRelative's var2 has lower 16 bits for object
//    name). In port mode, when convertible to Int, we should isolate lower 16 and emit Upper16 | MT_<id>.
//  - Support comments in SOC.
//  - Add help text and readme.
//  - Add option to try to follow action variables
//  - Add no-recurse option
//  - Add option to convert some fields into terms of frac units and flags.
//  - Add upgrade option to attempt to migrate old actions to new versions
//  - Support port ID patching for more action types (var1 and var2)
//  - Add external sound dependencies to listing.
//  - Print dependencies for SOC format.

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
  val freeslotPrefix = argValue(Set("--freeslot-prefix")).getOrElse("")
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

    def adjustHardcodedSlot(adjustment: Int => String)(id: String): Option[String] = {
      Try(id.toInt) match {
        case Success(0) => Some("0") // 0 is special case for null. Don't adjust!
        case Success(numericId) => Some(adjustment(numericId))
        case _ => None
      }
    }

    def generateSlotName(hardcodedSlot: Int, maxLength: Int, minLength: Int = 0, radix: Int = 10) = {
      val asBigInt: BigInt = hardcodedSlot
      val radixEncodedId = asBigInt.toString(radix)

      if (radixEncodedId.length > maxLength)
        error("Hardcoded slot ID is too big to be auto-converted to freeslot name.")

      val prefix = freeslotPrefix.take(maxLength - radixEncodedId.length)
      val pad = "0" * (minLength - (prefix.length + radixEncodedId.length)) // Negative is ok!

      (prefix + pad + radixEncodedId).toUpperCase
    }

    val ported = if (portable) MakePortable(
      SlotRenameRules(
        thingId = id => adjustHardcodedSlot(s => s"MT_${generateSlotName(s, 20)}")(id),
        stateId = id => adjustHardcodedSlot(s => s"S_${generateSlotName(s, 20)}")(id),
        soundId = id => adjustHardcodedSlot(s => s"sfx_${generateSlotName(s, 6).toLowerCase}")(id),
        spriteId = id =>
          // Note: sprite IDs get base-36 encoded since they're only allowed to be 4 chars long!
          adjustHardcodedSlot(s => s"SPR_${generateSlotName(s, 4, minLength = 4, radix = 36)}")(id)
      )
    )(extracted) else extracted

    val withDependencyInfo = BuildDependencyInfo(ported)

    val print: SocScript => Unit = if (toLua)
      PrintAsLua(PrinterConfig())
    else
      PrintAsSoc(PrinterConfig())

    /* print extracted blocks to stdout */
    print(withDependencyInfo)
  }

  doExtract()
}
