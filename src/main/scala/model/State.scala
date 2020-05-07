package model

case class State(
  id: String,
  spriteNumber: Option[String] = None,
  spriteSubNumber: Option[String] = None,
  duration: Option[String] = None,
  next: Option[String] = None,
  action: Option[String] = None,
  var1: Option[String] = None,
  var2: Option[String] = None,
) {
  def objects: Set[String] = Var1Object(true).toSet ++ Var2Object()
  def states: Set[String] = next.filterNot(_ == "0").toSet ++ Var1State() ++ Var2State()
  def sounds: Set[String] = Var1Sound().toSet ++ Var2Sound()

  // TODO:
  //  - Depends on object ID in RaiseState:
  //      A_DropMine
  //      A_ShootBullet
  //  - A_RandomStateRange uses a state range.
  //  - Warn user:
  //      A_CusValAction
  //      A_RelayCustomValue
  //      A_SetCustomValue
  //      A_UseCusValMemo



  /**
   * Note: we don't do anything special for upper v.s. lower
   * data, since we cannot resolve expressions (this would require all constants
   * and maybe even runtime values from SRB2). And a expression parser / evaluator.
   *
   * Instead, we just return the full Var text if either the upper or lower portion
   * should contain a reference.
   *
   * MakePortable will not patch values that aren't used to declare a local entity block,
   * so there's relatively little risk of it patching something incorrectly, assuming the
   * var value is stored in the lower 16 bits. If the value is expected in the upper 16,
   * we DO NOT patch it, since it'd be easy to get wrong (e.g. upper 16 is actually null,
   *
   * but lower has an unrelated value < 2^16 which could be misinterpreted).
   *
   * @param includeUpper return expression even if action uses upper var1 to store object.
   *                     Used by dependency lists, but not port mode.
   * @return
   */
  def Var1Object(includeUpper: Boolean = false): Option[String] = {
    if (var1.contains("0")) return None

    // Special case for remote action
    if (action.contains("A_RemoteAction")) {
      if (var1.contains("-1") || var1.contains("-2")) return None
    }

    val upper = Set(
      "A_CheckThingCount",
      "A_MultiShot"
    )

    if (!includeUpper && action.exists(upper)) {
      // We don't include upper when porting, since we cannot know the value
      // without evaluating expressions.
      return None
    }

    val lower = Set(
      "A_FindTarget",
      "A_OldRingExplode",
      "A_FindTracer",
      "A_FindTarget",
      "A_SharpSpin",
      "A_Boss1Laser",
      "A_Boss7FireMissiles",
      "A_Boss4Raise",
      "A_BossFireShot",
      "A_BrakFireShot",
      "A_BrakLobShot",
      "A_ParticleSpawn",
      "A_SmokeTrailer",
      "A_FireShot",
      "A_LobShot",
      "A_MissileSplit",
      "A_NapalmScatter",
      "A_SuperFireShot",
      "A_SuperTurretFire",
      "A_TrapShot",
      "A_TurretFire",
      "A_VileTarget",
      "A_RemoteAction"
    )

    action match {
      case Some(a) if (upper ++ lower).contains(a) => var1
      case _ => None
    }
  }

  def Var1State(): Option[String] = {
    if (var1.contains("0")) return None

    val lower = Set(
      "A_MinusCheck",
      "A_DualAction",
      "A_RandomState",
      "A_RandomStateRange",
      "A_SetObjectState",
      "A_CusValAction"
    )

    action match {
      case Some(a) if lower.contains(a) => var1
      case _ => None
    }
  }

  def Var1Sound(): Option[String] = {
    if (var1.contains("0")) return None

    val lower = Set(
      "A_Boss4SpeedUp",
      "A_Boss4Reverse",
      "A_Boss4Raise",
      "A_BuzzFly",
      "A_VileAttack",
      "A_VileFire",
      "A_PlaySound"
    )

    action match {
      case Some(a) if lower.contains(a) => var1
      case _ => None
    }
  }

  def Var2Object(): Option[String] = {
    if (var2.contains("0")) return None

    val lower = Set(
      "A_SpawnObjectRelative",
      "A_BossScream",
      "A_SpawnObjectAbsolute",
      "A_SplitShot",
      "A_VileAttack",
      "A_VileFire"
    )

    action match {
      case Some(a) if lower.contains(a) => var2
      case _ => None
    }
  }

  def Var2State(): Option[String] = {
    if (var2.contains("0")) return None

    val lower = Set(
      "A_DualAction",
      "A_RandomState",
      "A_RandomStateRange",
      "A_RemoteAction",
      "A_SetFuse",
      "A_CheckAmbush",
      "A_CheckHealth",
      "A_CheckHeight",
      "A_CheckRandom",
      "A_CheckRange",
      "A_CheckRings",
      "A_CheckTargetRings",
      "A_CheckTotalRings",
      "A_CheckTrueRange",
      "A_SearchForPlayers",
      "A_CheckCustomValue",
      "A_CheckCusValMemo",
      "A_Repeat"
    )

    action match {
      case Some(a) if lower.contains(a) => var2
      case _ => None
    }
  }

  def Var2Sound(): Option[String] = {
    if (var2.contains("0")) return None

    val lower = Set(
      "A_Boss7FireMissiles",
      "A_Boss4Reverse",
      "A_BrakChase"
    )

    action match {
      case Some(a) if lower.contains(a) => var2
      case _ => None
    }
  }

  def Var1LinedefExecutor(): Option[String] = {
    if (var1.contains("0")) return None

    val actions = Set(
      "A_LinedefExecute"
    )

    var1.filter(actions.contains)
  }
}