object Upgrade {
  def apply(socScript: SocScript): SocScript = {
    socScript.copy(states = socScript.states.mapValues(entry => {
      val state = entry.entity
      val upgradedAction = state.action match {
        case Some("A_RingExplode") => state.var1 match {
          case Some(var1) if var1 != "0" => Some("A_OldRingExplode")
          case _ => None
        }
        case _ => state.action
      }

      entry.copy(entity = state.copy(action = upgradedAction))
    }))
  }
}
