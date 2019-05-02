package parser

import org.scalatest.FlatSpec

class ThingBlockSpec extends FlatSpec {
  it should "parse states with filter property" in {
    val thingId = 123

    val meleeStateKey = "MELEESTATE"
    val meleeStateId = 111

    val seeStateKey = "SeeStAte"
    val seeStateId = 222

    val lines = List(
      s"Thing $thingId",
      s"$meleeStateKey = $meleeStateId",
      s"$seeStateKey = $seeStateId"
    )

    val thingOpt = ThingBlock.unapply(lines)
    assert(thingOpt.isDefined)

    val thing = thingOpt.get
    assert(thing.states(meleeStateKey) == meleeStateId)
    assert(thing.states(seeStateKey) == seeStateId)
  }

  it should "parse sounds with filter property" in {
    val thingId = 123

    val deathSoundKey = "DEATHSOUND"
    val deathSoundId = 111

    val seeSoundKey = "SeeSounD"
    val seeSoundId = 222

    val lines = List(
      s"Thing $thingId",
      s"$deathSoundKey = $deathSoundId",
      s"$seeSoundKey = $seeSoundId"
    )

    val thingOpt = ThingBlock.unapply(lines)
    assert(thingOpt.isDefined)

    val thing = thingOpt.get
    assert(thing.sounds(deathSoundKey) == deathSoundId)
    assert(thing.sounds(seeSoundKey) == seeSoundId)
  }
}
