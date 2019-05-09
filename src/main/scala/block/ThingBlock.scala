package block

import model.Thing

object ThingBlock extends Block[Thing] {
  import Line.{KeyExactly, EqualsDelimiter, KeyFilter}

  object HeaderLine extends Block.HeaderLine("THING")

  object MapThingNumLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "MAPTHINGNUM"
  }

  object SpawnStateLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "SPAWNSTATE"
  }

  object SpawnHealthLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "SPAWNHEALTH"
  }

  object SeeStateLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "SEESTATE"
  }

  object SeeSoundLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "SEESOUND"
  }

  object ReactionTimeLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "REACTIONTIME"
  }

  object AttackSoundLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "ATTACKSOUND"
  }

  object PainStateLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "PAINSTATE"
  }

  object PainChanceLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "PAINCHANCE"
  }

  object PainSoundLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "PAINSOUND"
  }

  object MeleeStateLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "MELEESTATE"
  }

  object MissileStateLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "MISSILESTATE"
  }

  object DeathStateLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "DEATHSTATE"
  }

  object DeathSoundLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "DEATHSOUND"
  }

  object XDeathStateLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "XDEATHSTATE"
  }

  object SpeedLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "SPEED"
  }

  object RadiusLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "RADIUS"
  }

  object HeightLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "HEIGHT"
  }

  object MassLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "MASS"
  }

  object DamageLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "DAMAGE"
  }

  object ActiveSoundLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "ACTIVESOUND"
  }

  object RaiseStateLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "RAISESTATE"
  }

  object FlagsLine extends Line.Distinct[Int]
    with EqualsDelimiter
    with KeyExactly
  {
    override val keyName: String = "FLAGS"
  }

  object StateDependencyLine extends Line.Like[Int]
    with EqualsDelimiter
    with KeyFilter
  {
    override def keyFilter(key: String): Boolean = key.toUpperCase.endsWith("STATE")
  }

  object SoundDependencyLine extends Line.Like[Int]
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
      thing.mass         .map(MassLine(_)),
      thing.damage       .map(DamageLine(_)),
      thing.activeSound  .map(ActiveSoundLine(_)),
      thing.raiseState   .map(RaiseStateLine(_)),
      thing.flags        .map(FlagsLine(_))
    ).flatten
  }
}
