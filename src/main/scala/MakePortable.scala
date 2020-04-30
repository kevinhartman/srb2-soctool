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

    def patchProp(prop: Option[String], renameRule: String => Option[String]) = {
      prop match {
        case Some("0") => Some("0")
        case Some(id) => Some(renameRule(id).getOrElse(id))
        case None => None
      }
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
      entry.copy(entity = state.copy(
        id = slotRenameRules.stateId(state.id).getOrElse(state.id),
        next = patchState(state.next),
        spriteNumber = patchSprite(state.spriteNumber),
        var1 = state.action match {
          case Some("A_FindTarget") => patchThing(state.var1)
          case Some("A_OldRingExplode") => patchThing(state.var1)
          case _ => state.var1
        },
        var2 = state.action match {
          case Some("A_SpawnObjectRelative") => patchThing(state.var2)
          case Some("A_BossScream") => patchThing(state.var2)
          case _ => state.var2
        }
      ))}
    ).map(entry => (entry.entity.id, entry))(breakOut)

    socScript.copy(things = patchThings, states = patchStates, sounds = patchSounds)
  }
}
