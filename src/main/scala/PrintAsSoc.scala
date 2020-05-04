import block.LevelBlock
import block._

object PrintAsSoc {
  def apply(config: PrinterConfig)(socScript: SocScript): Unit = {
    def printComment(msg: String = "") = println(s"# $msg")
    def printDependency(dep: String) = println(s"# - $dep")

    def printDependencies() = {
      printComment("Dependencies")
      printComment()
      printComment("Required sound files:")
      socScript.dependencies.soundsFiles.foreach(printDependency)
      printComment()
      printComment("Required sprite files (see https://wiki.srb2.org/wiki/Sprite for info on rotation):")
      socScript.dependencies.spriteFiles.foreach(printDependency)
      printComment()
      printComment("Linedef executors:")
      socScript.dependencies.lineDefs.foreach(printDependency)
      printComment()
      printComment("External object references:")
      socScript.dependencies.externObjects.foreach(printDependency)
      printComment()
      printComment("External state references:")
      socScript.dependencies.externStates.foreach(printDependency)
      printComment()
      printComment("External sprite references:")
      socScript.dependencies.externSprites.foreach(printDependency)
      printComment()
      printComment("External sound references:")
      socScript.dependencies.externSounds.foreach(printDependency)
      printComment()
      println()
    }

    def printWarnings[T](entry: Entry[T]) = {
      entry.warnings.foreach(w => println(s"# info: $w"))
    }

    printDependencies()

    if (socScript.freeSlots.nonEmpty) {
      println("Freeslot")
      socScript.freeSlots.toSeq.sorted.foreach(println)
      println()
    }

    socScript.levels.values.filter(config.levelFilter).foreach(entry => {
      printWarnings(entry)
      LevelBlock(entry.entity).foreach(println)
      println()
    })

    socScript.objects.values.filter(config.objectFilter).foreach(entry => {
      printWarnings(entry)
      ObjectBlock(entry.entity).foreach(println)
      println()
    })

    socScript.states.values.filter(config.stateFilter).foreach(entry => {
      printWarnings(entry)
      StateBlock(entry.entity).foreach(println)
      println()
    })

    socScript.sounds.values.filter(config.soundFilter).foreach(entry => {
      printWarnings(entry)
      SoundBlock(entry.entity).foreach(println)
      println()
    })
  }
}