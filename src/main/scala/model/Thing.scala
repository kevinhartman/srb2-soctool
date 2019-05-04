package model

case class Thing(
  id: Int,
  mapThingNum: Option[Int] = None,
  spawnState: Option[Int] = None,
  spawnHealth: Option[Int] = None,
  seeState: Option[Int] = None,
  seeSound: Option[Int] = None,
  reactionTime: Option[Int] = None,
  attackSound: Option[Int] = None,
  painState: Option[Int] = None,
  painChance: Option[Int] = None,
  painSound: Option[Int] = None,
  meleeState: Option[Int] = None,
  missilesState: Option[Int] = None,
  deathState: Option[Int] = None,
  deathSound: Option[Int] = None,
  xDeathState: Option[Int] = None,
  speed: Option[Int] = None,
  radius: Option[Int] = None,
  height: Option[Int] = None,
  mass: Option[Int] = None,
  damage: Option[Int] = None,
  activeSound: Option[Int] = None,
  raiseState: Option[Int] = None,
  flags: Option[Int] = None,

  states: Set[Int] = Set(),
  sounds: Set[Int] = Set()
)
{
  def withState(stateId: Int): Thing =
    this.copy(states = this.states + stateId)

  def withSound(soundId: Int): Thing =
    this.copy(sounds = this.sounds + soundId)
}
