package model

import org.scalatest.FlatSpec

class SocScriptSpec extends FlatSpec {

  it should "handle an empty file" in {
    val lines = List("")
    val script = SocScript(lines)
    assert(script.levels.isEmpty)
    assert(script.things.isEmpty)
    assert(script.states.isEmpty)
    assert(script.sounds.isEmpty)
  }

  it should "handle states" in {
    val stateId = 1
    val nextId = 999
    val actionName = "a_Test"

    val lines = List(
      "",
      "",
      s"State $stateId",
      "FAKE VALUE",
      s"NEXT = $nextId",
      s"ACTION $actionName",
      "FAKE = VALUE",
      "NEXT = fake",
      "",
      ""
    )

    val script = SocScript(lines)
    assert(script.states(stateId).entity == State(
      length = 6,
      next = Some(nextId),
      action = Some(actionName)
    ))
  }
}
