package parser

import model._

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
    val stateEntry = script.states(stateId)

    // Check that the entity was properly parsed
    assert(stateEntry.entity == State(
      id = stateId,
      next = Some(nextId),
      action = Some(actionName)
    ))

    // Check that the script entry info is correct
    assert(stateEntry.length == 6)
    assert(stateEntry.offset == 2)
  }

  it should "handle a script with a single comment" in {
    val lines = List(
      "# comment"
    )

    SocScript(lines)
  }

  it should "handle a script containing multiple blocks, some of the same type" in {
    val lines = List(
      "Thing 1",
      "",
      "State 1",
      "",
      "Thing 2",
      "",
      "State 2",
      "",
      "Sound 1",
      "",
      "Level 0"
    )

    val script = SocScript(lines)

    assert(script.things.size == 2)
    assert(script.states.size == 2)
    assert(script.sounds.size == 1)
    assert(script.levels.size == 1)
  }
}
