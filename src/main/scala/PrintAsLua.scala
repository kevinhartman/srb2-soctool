import block.LevelBlock
import block._

object PrintAsLua {
  def apply(config: PrinterConfig)(socScript: SocScript): Unit = {
    def printProp(prop: String) = println(s"    $prop")

    if (socScript.freeSlots.nonEmpty) {
      println("freeslots(")
      val sorted = socScript.freeSlots.map(_.slotId).toIndexedSeq.sorted(Ordering[String].reverse)
      sorted.tails.toSeq.drop(1).headOption.toSeq.flatten.reverse.foreach(s => printProp(s"$s,"))
      printProp(sorted.head)
      println(")")
      println()
    }

    socScript.things.values.filter(config.thingFilter).foreach(entry => {
      val thing = entry.entity

      println(s"mobjinfo[${thing.id}] = {")
      thing.mapThingNum.map(s => s"doomednum = $s,").foreach(printProp)
      thing.spawnState.map(s => s"spawnstate = $s,").foreach(printProp)
      thing.spawnHealth.map(s => s"spawnhealth = $s,").foreach(printProp)
      thing.seeState.map(s => s"seestate = $s,").foreach(printProp)
      thing.seeSound.map(s => s"seesound = $s,").foreach(printProp)
      thing.reactionTime.map(s => s"reactiontime = $s,").foreach(printProp)
      thing.attackSound.map(s => s"attacksound = $s,").foreach(printProp)
      thing.painState.map(s => s"painstate = $s,").foreach(printProp)
      thing.painChance.map(s => s"painchance = $s,").foreach(printProp)
      thing.painSound.map(s => s"painsound = $s,").foreach(printProp)
      thing.meleeState.map(s => s"meleestate = $s,").foreach(printProp)
      thing.missilesState.map(s => s"missilestate = $s,").foreach(printProp)
      thing.deathState.map(s => s"deathstate = $s,").foreach(printProp)
      thing.xDeathState.map(s => s"xdeathstate = $s,").foreach(printProp)
      thing.deathSound.map(s => s"deathsound = $s,").foreach(printProp)
      thing.speed.map(s => s"speed = $s,").foreach(printProp)
      thing.radius.map(s => s"radius = $s,").foreach(printProp)
      thing.height.map(s => s"height = $s,").foreach(printProp)
      thing.mass.map(s => s"mass = $s,").foreach(printProp)
      thing.dispOffset.map(s => s"dispoffset = $s,").foreach(printProp)
      thing.damage.map(s => s"damage = $s,").foreach(printProp)
      thing.activeSound.map(s => s"activesound = $s,").foreach(printProp)
      thing.flags.map(s => s"flags = $s,").foreach(printProp)
      thing.raiseState.map(s => s"raisestate = $s").foreach(printProp)
      println("}")
      println()
    })

    socScript.states.values.filter(config.stateFilter).foreach(entry => {
      val state = entry.entity

      println(s"states[${state.id}] = {")
      state.spriteNumber.map(s => s"sprite = $s,").foreach(printProp)
      state.spriteSubNumber.map(s => s"frame = $s,").foreach(printProp)
      state.duration.map(s => s"tics = $s,").foreach(printProp)
      state.action.map(s => s"action = $s,").foreach(printProp)
      state.var1.map(s => s"var1 = $s,").foreach(printProp)
      state.var2.map(s => s"var2 = $s,").foreach(printProp)
      state.next.map(s => s"nextstate = $s,").foreach(printProp)
      println("}")
      println()
    })

    socScript.sounds.values.filter(config.soundFilter).foreach(entry => {
      val sound = entry.entity

      println(s"S_sfx[${sound.id}] = {")
      sound.singular.map(s => s"singular = $s,").foreach(printProp)
      sound.priority.map(s => s"priority = $s,").foreach(printProp)
      sound.flags.map(s => s"flags = $s,").foreach(printProp)
      println("}")
      println()
    })
  }
}