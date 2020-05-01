import model.{Sound, State, Thing}

case class SlotRenameRules(
  thingId: String => Option[String] = _ => None,
  stateId: String => Option[String] = _ => None,
  soundId: String => Option[String] = _ => None,
  spriteId: String => Option[String] = _ => None
)

object MakePortable {
  def apply(slotRenameRules: SlotRenameRules)(socScript: SocScript): SocScript = {
    import scala.collection.breakOut

    val things = socScript.things.values.map(_.entity)
    val sounds = socScript.sounds.values.map(_.entity)
    val states = socScript.states.values.map(_.entity)

    val freeslots: Set[String] =
      things.flatMap(thing => slotRenameRules.thingId(thing.id)).toSet ++
      sounds.flatMap(sound => slotRenameRules.soundId(sound.id)) ++
      states.flatMap(state => slotRenameRules.stateId(state.id)) ++
      states.flatMap(_.spriteNumber).flatMap(spriteId => slotRenameRules.spriteId(spriteId))

    def patchProp(prop: Option[String], renameRule: String => Option[String]) = {
      prop.map(id => renameRule(id).filter(freeslots.contains).getOrElse(id))
      // TODO: log if skipped
    }

    def patchThing(prop: Option[String]) = patchProp(prop, slotRenameRules.thingId)
    def patchSound(prop: Option[String]) = patchProp(prop, slotRenameRules.soundId)
    def patchState(prop: Option[String]) = patchProp(prop, slotRenameRules.stateId)
    def patchSprite(prop: Option[String]) = patchProp(prop, slotRenameRules.spriteId)

    val patchThings: Map[String, Entry[Thing]] = socScript.things.values.map(entry => {
      val thing = entry.entity
      entry.copy(entity = thing.copy(
        id = slotRenameRules.thingId(thing.id).getOrElse(thing.id),
        sounds = thing.sounds.map(s => slotRenameRules.soundId(s).getOrElse(s)),
        states = thing.states.map(s => slotRenameRules.stateId(s).getOrElse(s)),

        seeSound = patchSound(thing.seeSound),
        activeSound = patchSound(thing.activeSound),
        deathSound = patchSound(thing.deathSound),
        painSound = patchSound(thing.painSound),
        attackSound = patchSound(thing.attackSound),

        deathState = patchState(thing.deathState),
        meleeState = patchState(thing.meleeState),
        missilesState = patchState(thing.missilesState),
        painState = patchState(thing.painState),
        raiseState = patchState(thing.raiseState),
        seeState = patchState(thing.seeState),
        spawnState = patchState(thing.spawnState),
        xDeathState = patchState(thing.xDeathState)
      ))}
    ).map(entry => (entry.entity.id, entry))(breakOut)

    val patchSounds: Map[String, Entry[Sound]] = socScript.sounds.values.map(entry => {
      val sound = entry.entity
      entry.copy(entity = sound.copy(
        id = slotRenameRules.soundId(sound.id).getOrElse(sound.id)
      ))}
    ).map(entry => (entry.entity.id, entry))(breakOut)

    val patchStates: Map[String, Entry[State]] = socScript.states.values.map (entry => {
      val state = entry.entity

      val patchedSprite = patchSprite(state.spriteNumber)
      val warnings = patchedSprite match {
        case Some(patchedId) if freeslots.contains(patchedId) =>
          entry.warnings :+ s"sprite freeslot '$patchedId' migrated from slot ${state.spriteNumber.get}"
        case _ => entry.warnings
      }

      entry.copy(
        warnings = warnings,
        entity = state.copy(
          id = slotRenameRules.stateId(state.id).getOrElse(state.id),
          next = patchState(state.next),
          spriteNumber = patchedSprite,
          var1 = patchThing(state.Var1AsThing()).orElse(state.var1),
          var2 = patchThing(state.Var2AsThing()).orElse(state.var2)
        )
      )
    }).map(entry => (entry.entity.id, entry))(breakOut)

    socScript.copy(freeSlots = freeslots, things = patchThings, states = patchStates, sounds = patchSounds)
  }
}
