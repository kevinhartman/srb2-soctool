import model.{Sound, State, Object}

case class SlotRenameRules(
  objectId: String => Option[String] = _ => None,
  stateId: String => Option[String] = _ => None,
  soundId: String => Option[String] = _ => None,
  spriteId: String => Option[String] = _ => None
)

object MakePortable {
  def apply(slotRenameRules: SlotRenameRules)(socScript: SocScript): SocScript = {
    import scala.collection.breakOut

    val objects = socScript.objects.values.map(_.entity)
    val sounds = socScript.sounds.values.map(_.entity)
    val states = socScript.states.values.map(_.entity)

    // This bit of code builds the list of generated freeslot names.
    // References to hard-coded slots are only patched if their would-be ported
    // name identifies one of these freeslots.
    //
    // The logic is:
    //   - for objects, states, and sounds, generate a freeslot if the original
    //     slot ID is a hard-coded number.
    //   - for sprites, generate a freeslot only if:
    //       a) the sprite ID is hard-coded AND
    //       b) at least one state using it was also declared with a hard-coded ID.
    val freeslots: Set[String] =
      objects.flatMap(obj => slotRenameRules.objectId(obj.id)).toSet ++
      sounds.flatMap(sound => slotRenameRules.soundId(sound.id)) ++
      states.flatMap(state => slotRenameRules.stateId(state.id)) ++
      states.filter(state => slotRenameRules.stateId(state.id).isDefined)
        .flatMap(_.spriteNumber)
        .flatMap(spriteId => slotRenameRules.spriteId(spriteId))

    def patchProp(prop: Option[String], renameRule: String => Option[String]) = {
      prop.map(id => renameRule(id).filter(freeslots.contains).getOrElse(id))
      // TODO: log if skipped
    }

    def patchObject(prop: Option[String]) = patchProp(prop, slotRenameRules.objectId)
    def patchSound(prop: Option[String]) = patchProp(prop, slotRenameRules.soundId)
    def patchState(prop: Option[String]) = patchProp(prop, slotRenameRules.stateId)
    def patchSprite(prop: Option[String]) = patchProp(prop, slotRenameRules.spriteId)

    val patchObjects: Map[String, Entry[Object]] = socScript.objects.values.map(entry => {
      val obj = entry.entity
      entry.copy(entity = obj.copy(
        id = slotRenameRules.objectId(obj.id).getOrElse(obj.id),
        sounds = obj.sounds.map(s => slotRenameRules.soundId(s).getOrElse(s)),
        states = obj.states.map(s => slotRenameRules.stateId(s).getOrElse(s)),

        seeSound = patchSound(obj.seeSound),
        activeSound = patchSound(obj.activeSound),
        deathSound = patchSound(obj.deathSound),
        painSound = patchSound(obj.painSound),
        attackSound = patchSound(obj.attackSound),

        deathState = patchState(obj.deathState),
        meleeState = patchState(obj.meleeState),
        missilesState = patchState(obj.missilesState),
        painState = patchState(obj.painState),
        raiseState = patchState(obj.raiseState),
        seeState = patchState(obj.seeState),
        spawnState = patchState(obj.spawnState),
        xDeathState = patchState(obj.xDeathState)
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
          var1 = patchObject(state.Var1AsObject()).orElse(state.var1),
          var2 = patchObject(state.Var2AsObject()).orElse(state.var2)
        )
      )
    }).map(entry => (entry.entity.id, entry))(breakOut)

    socScript.copy(freeSlots = freeslots, objects = patchObjects, states = patchStates, sounds = patchSounds)
  }
}
