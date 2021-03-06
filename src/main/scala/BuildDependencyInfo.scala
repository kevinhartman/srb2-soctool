import scala.util.Try

object BuildDependencyInfo {
  def apply(socScript: SocScript): SocScript = {
    def isHardcoded(id: String) = Try(id.toInt).isSuccess

    def soundName(id: String) = {
      s"DS${id.drop(4).toUpperCase}"
    }

    def spriteName(id: String, subNum: String) = {
      val frameNum = subNum.toInt

//      val frameId = subNum.toInt match {
//        case upper if upper >= 0 && upper < 26 => (upper + 64).toChar
//        case digit if digit >= 26 && digit < 36 => (digit + 48).toChar
//        case lower if lower >= 36 && lower < 62 => (lower + 97).toChar
//        case bang if bang == 62 => '!'
//        case at if at == 63 => '@'
//      }
      val frameId = "<frameid>"

      s"${id.drop(4)}$frameId<rotation>"
    }

    val objects = socScript.objects.values.map(_.entity)
    val states = socScript.states.values.map(_.entity)
    val sounds = socScript.sounds.values.map(_.entity)

    val processObjects = objects.foldLeft(Dependencies())((deps, obj) => {
      deps.copy(
        externStates = deps.externStates ++ obj.states.filterNot(socScript.states.contains).filterNot(_ == "0"),
        externSounds = deps.externSounds ++ obj.sounds.filterNot(socScript.sounds.contains).filterNot(_ == "0"),

        // We mention the sound file if it has a local declaration, as long as it's not hardcoded
        // TODO: support hard coded sound names.
        soundsFiles = deps.soundsFiles ++ obj.sounds.filter(socScript.sounds.contains).filterNot(isHardcoded).map(soundName)
      )
    })

    val processStates = states.foldLeft(processObjects)((deps, state) => {
      deps.copy(
        externStates = deps.externStates ++ state.states.filterNot(socScript.states.contains),
        externSprites = deps.externSprites ++ state.spriteNumber
          .filterNot(socScript.freeSlots.contains),
        spriteFiles = deps.spriteFiles ++ state.spriteNumber
          .filter(socScript.freeSlots.contains)
          .map(spriteName(_, state.spriteSubNumber.getOrElse("0"))),
        externObjects = deps.externObjects ++ state.objects.filterNot(socScript.objects.contains),
        externSounds = deps.externSounds ++ state.sounds.filterNot(socScript.sounds.contains),
        lineDefs = deps.lineDefs ++ state.Var1LinedefExecutor()
      )
    })

    val processSounds = sounds.filterNot(e => isHardcoded(e.id)).foldLeft(processStates)((deps, sound) => {
        deps.copy(
          soundsFiles = deps.soundsFiles + soundName(sound.id)
        )
    })

    socScript.copy(dependencies = processSounds)
  }
}
