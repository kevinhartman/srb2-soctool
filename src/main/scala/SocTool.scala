import scala.collection.mutable
import scala.io.Source
import scala.util.{Success, Try}

// TODO:
//  - Add no-recurse option
//  - Implement frame ID in required sprite file list.
//  - It'd be cleaner to return None instead of 0 if a prop has a special case null val for 0 (most props)
//  - Lots of missing properties on Level.
//  - We should list required sound files for hard-coded number slots that have a local Sound def.
//    Will need to look through SRB2 src to figure out how these are named.
//  - Validate user freeslot prefix.
//  ---
//  Stretch
//  - Support upper 16 references when the full 32 bits of the var is an int.
//  - Support comments in SOC.
//  - Add option to not recursively select through var1 and var2.
//  - Add option to convert some fields into terms of frac units and flags.
//  - Add flag to avoid upgrading referenced sprites.
//  - Add option to force upgrade sounds even if they don't have a local definition.

object SocTool extends App {
  val attribution = Seq(
    "",
    "Generated with <3 by @ctr_peach",
    "https://github.com/kevinhartman/srb2-soctool"
  )

  def error(message: String) = {
    throw new Exception(s"** Error: $message")
  }

  // Okay, this isn't "functionally pure".
  // Everything else in this codebase is, though (for better or worse).
  val knownArgs: mutable.Set[String] = mutable.Set()

  def argValue(argNames: Set[String]): Option[String] = {
    knownArgs ++= argNames
    args.dropWhile(arg => !argNames.contains(arg)).drop(1).headOption
  }

  def argFlag(argNames: Set[String]): Boolean = {
    knownArgs ++= argNames
    args.dropWhile(arg => !argNames.contains(arg)).nonEmpty
  }

  def toIdList(arg: Option[String]) = {
    arg.getOrElse("").split(',').filter(_.nonEmpty)
  }

  val help = argFlag(Set("--help", "-?", "-h"))
  val noRecurse = argFlag(Set("--no-recurse", "-R"))
  val objects = toIdList(argValue(Set("--object-ids", "-o")))
  val states = toIdList(argValue(Set("--state-ids", "-s")))
  val sounds = toIdList(argValue(Set("--sound-ids", "-d")))
  val levels = toIdList(argValue(Set("--level-ids", "-l")))

  val socFile = argValue(Set("--input-soc", "-i"))

  val fromOld = argFlag(Set("--from-old-srb2", "-u"))
  val toLua = argFlag(Set("--to-lua", "-S"))
  val portable = argFlag(Set("--make-portable", "-p"))
  val freeslotPrefix = argValue(Set("--freeslot-prefix", "-f")).getOrElse("")

  val noDescribe = argFlag(Set("--no-describe", "-D"))
  val noInlineComments = argFlag(Set("--no-inline-comments", "-I"))
  val noAttribution = PrintHelp(argFlag(Set("--no-attribution", "-A")))

  args.foreach(arg => {
    if (arg.startsWith("-") && !knownArgs.contains(arg)) {
      System.err.println(s"** Error: unknown argument $arg")
      PrintHelp()
      System.exit(1)
    }
  })

  if (noRecurse) {
    System.err.println(
      """--no-recurse isn't implemented.
        |...it should be relatively easy, but I haven't gotten to it. PRs welcome :)
        |""".stripMargin)
    System.exit(1)
  }

  if (help) {
    PrintHelp()
    System.exit(0)
  }

  val source = socFile match {
    case Some(path) => Source.fromFile(path)
    case None => Source.fromInputStream(System.in)
  }

  def loadSoc(): SocScript = {
    val lines = source.getLines()
    var script = SocScript(lines)

    if (fromOld) script = Upgrade(script)

    if (Seq(objects, states, sounds, levels).forall(_.isEmpty)) {
     // Return all if no filters.
     script
    } else {
      val withObjects = objects.foldLeft(SocScript())((soc, id) => script.extractObject(id, soc))
      val withStates = states.foldLeft(withObjects)((soc, id) => script.extractState(id, soc))
      val withSounds = sounds.foldLeft(withStates)((soc, id) => script.extractSound(id, soc))
      val withLevels = levels.foldLeft(withSounds)((soc, id) => script.extractLevel(id, soc))

      withLevels
    }
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

  val extracted = AddWarnings(loadSoc())
  val ported = if (portable) MakePortable(
    SlotRenameRules(
      objectId = id => adjustHardcodedSlot(s => s"MT_${generateSlotName(s, 20)}")(id),
      stateId = id => adjustHardcodedSlot(s => s"S_${generateSlotName(s, 20)}")(id),
      soundId = id => adjustHardcodedSlot(s => s"sfx_${generateSlotName(s, 6).toLowerCase}")(id),
      spriteId = id =>
        // Note: sprite IDs get base-36 encoded since they're only allowed to be 4 chars long!
        adjustHardcodedSlot(s => s"SPR_${generateSlotName(s, 4, minLength = 4, radix = 36)}")(id)
    )
  )(extracted) else extracted

  val withDependencyInfo = BuildDependencyInfo(ported)

  val printerConfig = PrinterConfig(
    printDependencies = !noDescribe,
    printInfoMessages = !noInlineComments,
    printAttribution = !noAttribution
  )

  val print: SocScript => Unit = if (toLua)
    PrintAsLua(printerConfig)
  else
    PrintAsSoc(printerConfig)

  /* print extracted blocks to stdout */
  print(withDependencyInfo)
}