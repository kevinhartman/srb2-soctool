import model.SocScript

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
    loadFile() match {
      case Some(file) =>
        val lines = file.getLines()
        val script = SocScript(lines.toSeq)
        println(script)

      case None => error("SOC file not found")
    }

  }

  action.map(_.toLowerCase) match {
    case Some("extract") => doExtract()
    case None => error("missing entity type")
  }
}
