import block.LevelBlock
import block._

object PrintAsSoc {
  def apply(config: PrinterConfig)(socScript: SocScript): Unit = {
    if (socScript.freeSlots.nonEmpty) {
      println("Freeslot")
      socScript.freeSlots.foreach(println)
      println()
    }

    socScript.levels.values.filter(config.levelFilter).foreach(entry => {
      LevelBlock(entry.entity).foreach(println)
      println()
    })

    socScript.things.values.filter(config.thingFilter).foreach(entry => {
      ThingBlock(entry.entity).foreach(println)
      println()
    })

    socScript.states.values.filter(config.stateFilter).foreach(entry => {
      StateBlock(entry.entity).foreach(println)
      println()
    })

    socScript.sounds.values.filter(config.soundFilter).foreach(entry => {
      SoundBlock(entry.entity).foreach(println)
      println()
    })
  }
}