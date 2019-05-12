import scala.io.Source

object SocTool extends App {

  def error(message: String) = {
    throw new Exception(s"** Error: $message")
  }

  def argValue(argNames: Set[String]): Option[String] =
    args.dropWhile(arg => !argNames.contains(arg)).drop(1).headOption

  val entityType = argValue(Set("--type", "-t"))
  val entityId = argValue(Set("--id", "-d"))
  val action = argValue(Set("--action", "-a"))
  val socFile = argValue(Set("--soc", "-s"))

  def loadFile(): Option[Source] = {
    val file = socFile.map(fileName => Source.fromFile(fileName))
    file
  }

  def doExtract(): Unit = {
    val entity = entityType.getOrElse(error("Missing entity type"))
    val id = entityId.getOrElse(error("Missing entity id")).toInt

    val extracted = loadFile() match {
      case Some(file) =>
        val lines = file.getLines()
        val script = SocScript(lines.toSeq)
        println(script)

        entity.toUpperCase match {
          case "THING" => script.extractThing(id)
          case "LEVEL" => script.extractLevel(id)
          case "STATE" => script.extractState(id)
          case "SOUND" => script.extractSound(id)
        }
      case None => error("SOC file not found")
    }

    /* print extracted blocks to stdout */
    println(extracted)
  }

  action.map(_.toLowerCase) match {
    case Some("extract") => doExtract()
    case None => error("missing entity type")
  }
}
