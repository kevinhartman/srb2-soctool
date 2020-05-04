object PrintAsLua {
  def apply(config: PrinterConfig)(socScript: SocScript): Unit = {
    def printAttribution(): Unit = {
      if (!config.printAttribution) return
      SocTool.attribution.foreach(println)
    }

    def printDependency(dep: String) = println(s"- $dep")

    def printDependencies(): Unit = {
      if (!config.printDependencies) return

      println("/*")
      println("Dependencies")
      println()
      println("Required sound files:")
      socScript.dependencies.soundsFiles.foreach(printDependency)
      println()
      println("Required sprite files (see https://wiki.srb2.org/wiki/Sprite for info on rotation):")
      socScript.dependencies.spriteFiles.foreach(printDependency)
      println()
      println("Linedef executors:")
      socScript.dependencies.lineDefs.foreach(printDependency)
      println()
      println("External object references:")
      socScript.dependencies.externObjects.foreach(printDependency)
      println()
      println("External state references:")
      socScript.dependencies.externStates.foreach(printDependency)
      println()
      println("External sprite references:")
      socScript.dependencies.externSprites.foreach(printDependency)
      println()
      println("External sound references:")
      socScript.dependencies.externSounds.foreach(printDependency)
      println()
      printAttribution()
      println("*/")
      println()
    }

    def printWarnings[T](entry: Entry[T]): Unit = {
      if (!config.printInfoMessages) return
      entry.warnings.foreach(w => println(s"-- info: $w"))
    }

    def printProp(prop: String) = println(s"    $prop")

    printDependencies()

    if (socScript.freeSlots.nonEmpty) {
      println("freeslot(")
      val sorted = socScript.freeSlots.toIndexedSeq.sorted(Ordering[String].reverse)
      sorted.tails.toSeq.drop(1).headOption.toSeq.flatten.reverse.foreach(s => printProp("\"" + s + "\","))
      printProp("\"" + sorted.head + "\"")
      println(")")
      println()
    }

    socScript.objects.values.filter(config.objectFilter).foreach(entry => {
      val obj = entry.entity

      printWarnings(entry)
      println(s"mobjinfo[${obj.id}] = {")
      obj.mapThingNum.map(s => s"doomednum = $s,").foreach(printProp)
      obj.spawnState.map(s => s"spawnstate = $s,").foreach(printProp)
      obj.spawnHealth.map(s => s"spawnhealth = $s,").foreach(printProp)
      obj.seeState.map(s => s"seestate = $s,").foreach(printProp)
      obj.seeSound.map(s => s"seesound = $s,").foreach(printProp)
      obj.reactionTime.map(s => s"reactiontime = $s,").foreach(printProp)
      obj.attackSound.map(s => s"attacksound = $s,").foreach(printProp)
      obj.painState.map(s => s"painstate = $s,").foreach(printProp)
      obj.painChance.map(s => s"painchance = $s,").foreach(printProp)
      obj.painSound.map(s => s"painsound = $s,").foreach(printProp)
      obj.meleeState.map(s => s"meleestate = $s,").foreach(printProp)
      obj.missilesState.map(s => s"missilestate = $s,").foreach(printProp)
      obj.deathState.map(s => s"deathstate = $s,").foreach(printProp)
      obj.xDeathState.map(s => s"xdeathstate = $s,").foreach(printProp)
      obj.deathSound.map(s => s"deathsound = $s,").foreach(printProp)
      obj.speed.map(s => s"speed = $s,").foreach(printProp)
      obj.radius.map(s => s"radius = $s,").foreach(printProp)
      obj.height.map(s => s"height = $s,").foreach(printProp)
      obj.mass.map(s => s"mass = $s,").foreach(printProp)
      obj.dispOffset.map(s => s"dispoffset = $s,").foreach(printProp)
      obj.damage.map(s => s"damage = $s,").foreach(printProp)
      obj.activeSound.map(s => s"activesound = $s,").foreach(printProp)
      obj.flags.map(s => s"flags = $s,").foreach(printProp)
      obj.raiseState.map(s => s"raisestate = $s").foreach(printProp)
      println("}")
      println()
    })

    socScript.states.values.filter(config.stateFilter).foreach(entry => {
      val state = entry.entity

      printWarnings(entry)
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

      printWarnings(entry)
      println(s"S_sfx[${sound.id}] = {")
      sound.singular.map(s => s"singular = $s,").foreach(printProp)
      sound.priority.map(s => s"priority = $s,").foreach(printProp)
      sound.flags.map(s => s"flags = $s,").foreach(printProp)
      println("}")
      println()
    })
  }
}