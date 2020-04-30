package model

case class Thing(
  id: String,
  mapThingNum: Option[String] = None,
  spawnState: Option[String] = None,
  spawnHealth: Option[String] = None,
  seeState: Option[String] = None,
  seeSound: Option[String] = None,
  reactionTime: Option[String] = None,
  attackSound: Option[String] = None,
  painState: Option[String] = None,
  painChance: Option[String] = None,
  painSound: Option[String] = None,
  meleeState: Option[String] = None,
  missilesState: Option[String] = None,
  deathState: Option[String] = None,
  deathSound: Option[String] = None,
  xDeathState: Option[String] = None,
  speed: Option[String] = None,
  radius: Option[String] = None,
  height: Option[String] = None,
  dispOffset: Option[String] = None,
  mass: Option[String] = None,
  damage: Option[String] = None,
  activeSound: Option[String] = None,
  raiseState: Option[String] = None,
  flags: Option[String] = None,

  states: Set[String] = Set(),
  sounds: Set[String] = Set()
)
{
  def withState(stateId: String): Thing =
    this.copy(states = this.states + stateId)

  def withSound(soundId: String): Thing =
    this.copy(sounds = this.sounds + soundId)
}
