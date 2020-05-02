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

    val things = socScript.things.values.map(_.entity)
    val states = socScript.states.values.map(_.entity)
    val sounds = socScript.sounds.values.map(_.entity)

    val processThings = things.foldLeft(Dependencies())((deps, thing) => {
      deps.copy(
        externStates = deps.externStates ++ thing.states.filterNot(socScript.states.contains).filterNot(_ == "0"),
        externSounds = deps.externSounds ++ thing.sounds.filterNot(socScript.sounds.contains).filterNot(_ == "0"),

        // even if the sound is not declared locally, we can know its name if not hard-coded
        soundsFiles = deps.soundsFiles ++ thing.sounds.filterNot(isHardcoded).map(soundName)
      )
    })

    val processStates = states.foldLeft(processThings)((deps, state) => {
      deps.copy(
        externStates = deps.externStates ++ state.next.filterNot(socScript.states.contains).filterNot(_ == "0"),
        externSprites = deps.externSprites ++ state.spriteNumber
          .filterNot(id => socScript.freeSlots.contains(id) || !isHardcoded(id)),
        spriteFiles = deps.spriteFiles ++ state.spriteNumber
          .filterNot(isHardcoded)
          .map(spriteName(_, state.spriteSubNumber.getOrElse("0"))),
        externObjects = deps.externObjects
          ++ state.Var1AsThing().filterNot(socScript.things.contains)
          ++ state.Var2AsThing().filterNot(socScript.things.contains),
        lineDefs = deps.lineDefs ++ state.Var1AsLinedefExecutor()
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
