package block

import org.scalatest.FlatSpec

class ObjectBlockSpec extends FlatSpec {
  it should "parse states with filter property" in {
    val objectId = "123"

    val meleeStateKey = "MELEESTATE"
    val meleeStateId = "111"

    val seeStateKey = "SeeStAte"
    val seeStateId = "222"

    val lines = List(
      s"Object $objectId",
      s"$meleeStateKey = $meleeStateId",
      s"$seeStateKey = $seeStateId"
    )

    val objectOpt = ObjectBlock.unapply(lines)
    assert(objectOpt.isDefined)

    val obj = objectOpt.get
    assert(obj.states.contains(meleeStateId))
    assert(obj.states.contains(seeStateId))
  }

  it should "parse sounds with filter property" in {
    val objectId = "123"

    val deathSoundKey = "DEATHSOUND"
    val deathSoundId = "111"

    val seeSoundKey = "SeeSounD"
    val seeSoundId = "222"

    val lines = List(
      s"Thing $objectId",
      s"$deathSoundKey = $deathSoundId",
      s"$seeSoundKey = $seeSoundId"
    )

    val objectOpt = ObjectBlock.unapply(lines)
    assert(objectOpt.isDefined)

    val obj = objectOpt.get
    assert(obj.sounds.contains(deathSoundId))
    assert(obj.sounds.contains(seeSoundId))
  }
}
