package block

import block.Line.{KeyIn, SpaceDelimiter, WellDefinedKey}
import model.Thing

object ThingBlock extends Block[Thing] {
  import Line.{KeyExactly, EqualsDelimiter, KeyFilter}

  object HeaderLine extends Line.Distinct[String]
    with SpaceDelimiter
    with KeyIn
    with WellDefinedKey
  {
    override val keyName: String = "Object"
    override val keys: Set[String] = Set("Object", "Thing")
  }

  object MapThingNumLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "MapThingNum"
  }

  object SpawnStateLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "SpawnState"
  }

  object SpawnHealthLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "SpawnHealth"
  }

  object SeeStateLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "SeeState"
  }

  object SeeSoundLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "SeeSound"
  }

  object ReactionTimeLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "ReactionTime"
  }

  object AttackSoundLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "AttackSound"
  }

  object PainStateLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "PainState"
  }

  object PainChanceLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "PainChance"
  }

  object PainSoundLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "PainSound"
  }

  object MeleeStateLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "MeleeState"
  }

  object MissileStateLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "MissileState"
  }

  object DeathStateLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "DeathState"
  }

  object DeathSoundLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "DeathSound"
  }

  object XDeathStateLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "XDeathState"
  }

  object SpeedLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "Speed"
  }

  object RadiusLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "Radius"
  }

  object HeightLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "Height"
  }

  object DispOffsetLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "DispOffset"
  }

  object MassLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "Mass"
  }

  object DamageLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "Damage"
  }

  object ActiveSoundLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "ActiveSound"
  }

  object RaiseStateLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "RaiseState"
  }

  object FlagsLine extends Line.Distinct[String]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "Flags"
  }

  object StateDependencyLine extends Line.Like[String]
    with EqualsDelimiter
    with KeyFilter
  {
    override def keyFilter(key: String): Boolean = key.toUpperCase.endsWith("STATE")
  }

  object SoundDependencyLine extends Line.Like[String]
    with EqualsDelimiter
    with KeyFilter
  {
    override def keyFilter(key: String): Boolean = key.toUpperCase.endsWith("SOUND")
  }

  override def parseHeader: PartialFunction[String, Thing] = {
    case HeaderLine(id) => Thing(
      id = id
    )
  }

  object Mux {
    def unapply(arg: String): Option[(String, String)] = Some((arg, arg))
  }

  def parseDependency(entry: Thing): PartialFunction[String, (String, Thing)] = {
    case Mux(SoundDependencyLine(id), line) => (line, entry.withSound(id))
    case Mux(StateDependencyLine(id), line) => (line, entry.withState(id))
    case Mux(_, line) => (line, entry)
  }

  def parseProperty: PartialFunction[(String, Thing), Thing] = {
    case (MapThingNumLine(num),      thing) => thing.copy(mapThingNum = Some(num))
    case (SpawnStateLine(stateId),   thing) => thing.copy(spawnState = Some(stateId))
    case (SpawnHealthLine(health),   thing) => thing.copy(spawnHealth = Some(health))
    case (SeeStateLine(stateId),     thing) => thing.copy(seeState = Some(stateId))
    case (SeeSoundLine(soundId),     thing) => thing.copy(seeSound = Some(soundId))
    case (ReactionTimeLine(time),    thing) => thing.copy(reactionTime = Some(time))
    case (AttackSoundLine(soundId),  thing) => thing.copy(attackSound = Some(soundId))
    case (PainStateLine(stateId),    thing) => thing.copy(painState = Some(stateId))
    case (PainChanceLine(chance),    thing) => thing.copy(painChance = Some(chance))
    case (PainSoundLine(soundId),    thing) => thing.copy(painSound = Some(soundId))
    case (MeleeStateLine(stateId),   thing) => thing.copy(meleeState = Some(stateId))
    case (MissileStateLine(stateId), thing) => thing.copy(missilesState = Some(stateId))
    case (DeathStateLine(stateId),   thing) => thing.copy(deathState = Some(stateId))
    case (DeathSoundLine(soundId),   thing) => thing.copy(deathSound = Some(soundId))
    case (XDeathStateLine(stateId),  thing) => thing.copy(xDeathState = Some(stateId))
    case (SpeedLine(speed),          thing) => thing.copy(speed = Some(speed))
    case (RadiusLine(radius),        thing) => thing.copy(radius = Some(radius))
    case (HeightLine(height),        thing) => thing.copy(height = Some(height))
    case (MassLine(mass),            thing) => thing.copy(mass = Some(mass))
    case (DispOffsetLine(offset),    thing) => thing.copy(dispOffset = Some(offset))
    case (DamageLine(damage),        thing) => thing.copy(damage = Some(damage))
    case (ActiveSoundLine(soundId),  thing) => thing.copy(activeSound = Some(soundId))
    case (RaiseStateLine(stateId),   thing) => thing.copy(raiseState = Some(stateId))
    case (FlagsLine(flags),          thing) => thing.copy(flags = Some(flags))
  }

  override def parseProperty(thing: Thing): PartialFunction[String, Thing] =
    parseDependency(thing).andThen(parseProperty)

  override def writeHeader(thing: Thing): String = HeaderLine(thing.id)

  override def writeProperties(thing: Thing): Seq[String] = {
    Seq(
      thing.mapThingNum  .map(MapThingNumLine(_)),
      thing.spawnState   .map(SpawnStateLine(_)),
      thing.spawnHealth  .map(SpawnHealthLine(_)),
      thing.seeState     .map(SeeStateLine(_)),
      thing.seeSound     .map(SeeSoundLine(_)),
      thing.reactionTime .map(ReactionTimeLine(_)),
      thing.attackSound  .map(AttackSoundLine(_)),
      thing.painState    .map(PainStateLine(_)),
      thing.painChance   .map(PainChanceLine(_)),
      thing.painSound    .map(PainSoundLine(_)),
      thing.meleeState   .map(MeleeStateLine(_)),
      thing.missilesState.map(MissileStateLine(_)),
      thing.deathState   .map(DeathStateLine(_)),
      thing.deathSound   .map(DeathSoundLine(_)),
      thing.xDeathState  .map(XDeathStateLine(_)),
      thing.speed        .map(SpeedLine(_)),
      thing.radius       .map(RadiusLine(_)),
      thing.height       .map(HeightLine(_)),
      thing.dispOffset   .map(DispOffsetLine(_)),
      thing.mass         .map(MassLine(_)),
      thing.damage       .map(DamageLine(_)),
      thing.activeSound  .map(ActiveSoundLine(_)),
      thing.raiseState   .map(RaiseStateLine(_)),
      thing.flags        .map(FlagsLine(_))
    ).flatten
  }
}
