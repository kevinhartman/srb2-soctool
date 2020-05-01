import block.LevelBlock
import block._

object PrintAsSoc {
  def apply(config: PrinterConfig)(socScript: SocScript): Unit = {
    def printWarnings[T](entry: Entry[T]) = {
      entry.warnings.foreach(w => println(s"# info: $w"))
    }

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

    socScript.things.values.filter(config.thingFilter).foreach(entry => {
      printWarnings(entry)
      ThingBlock(entry.entity).foreach(println)
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