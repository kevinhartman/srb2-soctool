import block.LevelBlock
import block._

object PrintAsSoc {
  def apply(config: PrinterConfig)(socScript: SocScript): Unit = {
    def printAttribution(): Unit = {
      if (!config.printAttribution) return
      SocTool.attribution.foreach(s => println(s"# $s"))
    }

    def printComment(msg: String = "") = println(s"# $msg")
    def printDependency(dep: String) = println(s"# - $dep")

    def printDependencies(): Unit = {
      if (!config.printDependencies) return

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
      printAttribution()
      println()
    }

    def printWarnings[T](entry: Entry[T]): Unit = {
      if (!config.printInfoMessages) return
      entry.warnings.foreach(ws => {
        val lines = ws.split('\n')

        println(s"# info: ${ lines.head }")
        lines.drop(1).foreach(w => println(s"#       $w")) }
      )
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