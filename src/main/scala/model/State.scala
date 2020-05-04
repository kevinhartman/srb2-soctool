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
  def Var1AsObject(): Option[String] = {
    if (var1.contains("0")) return None

    val actions = Seq(
      "A_FindTarget",
      "A_OldRingExplode"
    )

    action match {
      case Some(a) => if (actions.contains(a)) var1 else None
      case None => None
    }
  }

  def Var1AsLinedefExecutor(): Option[String] = {
    if (var1.contains("0")) return None

    val actions = Seq(
      "A_LinedefExecute"
    )

    action match {
      case Some(a) => if (actions.contains(a)) var1 else None
      case None => None
    }
  }

  def Var2AsObject(): Option[String] = {
    if (var2.contains("0")) return None

    val actions = Seq(
      "A_SpawnObjectRelative",
      "A_BossScream"
    )

    action match {
      case Some(a) => if (actions.contains(a)) var2 else None
      case None => None
    }
  }
}
