object AddWarnings {
  def apply(socScript: SocScript): SocScript = {
    socScript.copy(states = socScript.states.mapValues(entry => {
      val state = entry.entity

      val warnRaiseState = state.action.filter(a => Seq("A_DropMine", "A_ShootBullet").contains(a)).map(a =>
        s"""built-in action $a references an object in the RaiseState of its actors. soctool cannot parse this.
           |This means it won't show up in the dependency listing, and that it won't be patched in port mode.
           |Further, it's possible RaiseState will be patched incorrectly, if the object reference was an ID
           |that also happened to match that of a locally declared state.""".stripMargin
      )

      val warnStateRange = state.action.filter(_ == "A_RandomStateRange").map(a =>
        s"""built-in action $a uses a contiguous range of states. soctool doesn't support dependency analysis
           |for ranges of states, because this would require evaluation. If these states are locally declared,
           |expand the selector --state-ids to include these states manually.""".stripMargin
      )

      val warnAllBetsOff = state.action.filter(
        a => Seq("A_CusValAction", "A_RelayCustomValue", "A_SetCustomValue", "A_UseCusValMemo").contains(a)
      ).map(a =>
        s"""
           |built-in action $a is an abomination. soctool will make no attempt to perform dependency analysis
           |or porting for its potential references.""".stripMargin
      )

      val warnings = warnRaiseState ++ warnStateRange ++ warnAllBetsOff

      // Also warn to console, since these are important to workflow.
      warnings.foreach(ws => {
        val lines = ws.split('\n')

        System.err.println(s"* Warning: ${ lines.head }")
        lines.drop(1).foreach(w => System.err.println(s"           $w")) }
      )

      entry.copy(warnings = entry.warnings ++ warnings)

      // note: view.force causes mapValues to create a new map, which it normally (but unexpectedly) doesn't.
      // we need it to avoid multiple prints to stderr.
      //
      // there's a ticket filed on Scala for this API inconsistency: SI-4776
    }).view.force)
  }
}
