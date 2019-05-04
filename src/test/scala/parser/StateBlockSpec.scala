package parser

import model.State

import org.scalatest.FlatSpec

class StateBlockSpec extends FlatSpec {
  it should "parse header line" in {
    val expectedId = 123
    val line = s"State $expectedId"

    line match {
      case StateBlock.HeaderLine(stateId) => assert(stateId == expectedId)
      case _ => fail()
    }
  }

  it should "parse an ACTION line" in {
    val expectedAction = "a_Test"
    val line = s"ACTION $expectedAction"

    line match {
      case StateBlock.ActionLine(actionName) => assert(actionName == expectedAction)
      case _ => fail()
    }
  }

  it should "parse a NEXT line" in {
    val expectedId = 123
    val line = s"NEXT = $expectedId"

    line match {
      case StateBlock.NextLine(stateId) => assert(stateId == expectedId)
      case _ => fail()
    }
  }

  it should "fall through non-matches gracefully while parsing lines" in {
    val line = ""

    line match {
      case StateBlock.HeaderLine(_) => fail()
      case StateBlock.ActionLine(_) => fail()
      case StateBlock.NextLine(_) => fail()
      case _ =>
    }
  }

  it should "lines should match exactly one parser" in {
    /* exactly 1 matching line for each parser goes in `lines` */
    val lines = List(
      "State 123",
      "ACTION a_Test",
      "NEXT = 123"
    )

    lines.map(line => List(
      /* unapply for all parsers here to see if they conflict */
      StateBlock.HeaderLine.unapply(line),
      StateBlock.ActionLine.unapply(line),
      StateBlock.NextLine.unapply(line)
    )).foreach(list =>
      assert(list.count(_.isDefined) == 1)
    )
  }

  it should "parse empty State" in {
    val stateId = 123
    val lines = List(
      s"State $stateId"
    )

    val state = StateBlock.unapply(lines)
    assert(state.contains(
      State(
        id = stateId,
        next = None,
        action = None
      )
    ))
  }
}
