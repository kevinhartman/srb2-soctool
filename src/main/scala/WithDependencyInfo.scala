import scala.util.Try

object WithDependencyInfo {
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

    val processThings = socScript.things.values.map(_.entity).foldLeft(Dependencies())((deps, thing) => {
      deps.copy(
        externFreeslots = deps.externFreeslots ++ Seq(thing.id)
          .filterNot(isHardcoded)
          .filterNot(id => socScript.freeSlots.contains(FreeSlot(id))),

        externStates = deps.externStates ++ thing.states.filterNot(id => socScript.states.contains(id)),

        soundsFiles = deps.soundsFiles ++ thing.sounds.filterNot(isHardcoded).map(soundName)
      )
    })

    val processStates = socScript.states.values.map(_.entity).foldLeft(processThings)((deps, state) => {
      deps.copy(
        externStates = deps.externStates ++ state.next.filterNot(id => socScript.states.contains(id)),
        spriteFiles = deps.spriteFiles ++ state.spriteNumber
          .map(spriteName(_, state.spriteSubNumber.getOrElse("0"))),
        externObjects = deps.externObjects
          ++ state.Var1AsThing().filterNot(id => socScript.things.contains(id))
          ++ state.Var2AsThing().filterNot(id => socScript.things.contains(id)),
        lineDefs = deps.lineDefs ++ state.Var1AsLinedefExecutor()
      )
    })

    val processSounds = socScript.sounds.values.map(_.entity).foldLeft(processStates)((deps, sound) => {
      deps.copy(
        soundsFiles = deps.soundsFiles + soundName(sound.id)
      )
    })

    socScript.copy(dependencies = processSounds)
  }
}
