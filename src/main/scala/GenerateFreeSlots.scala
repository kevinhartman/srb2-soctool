import scala.util.{Failure, Success, Try}

object GenerateFreeSlots {
  def apply(socScript: SocScript): SocScript = {
    val slots = socScript.levels.keys ++ socScript.things.keys ++ socScript.states.keys ++ socScript.sounds.keys

    val result = slots.foldLeft(socScript)((soc, id) => {
      Try(id.toInt) match {
        case Success(_) => soc // not a free slot
        case Failure(_) => soc.withFreeSlot(FreeSlot(id))
          // TODO: would be a nice improvement to recognize and skip known official SRB2 slot constants
      }
    })

    socScript.states.values
      .flatMap(_.entity.spriteNumber)
      .filterNot(s => Try(s.toInt).isSuccess)
      .foldLeft(result)((soc, id) => soc.withFreeSlot(FreeSlot(id)))
  }
}
