import org.scalatest.FlatSpec

class StateSpec extends FlatSpec {
  it should "parse header line" in {
    val expectedId = 123
    val line = s"State $expectedId"

    line match {
      case State.Header(stateId) => assert(stateId == expectedId)
      case _ => fail()
    }
  }

  it should "parse an ACTION line" in {
    val expectedAction = "a_Test"
    val line = s"ACTION $expectedAction"

    line match {
      case State.Action(actionName) => assert(actionName == expectedAction)
      case _ => fail()
    }
  }

  it should "parse a NEXT line" in {
    val expectedId = 123
    val line = s"NEXT = $expectedId"

    line match {
      case State.Next(stateId) => assert(stateId == expectedId)
      case _ => fail()
    }
  }

  it should "fall through non-matches gracefully while parsing lines" in {
    val line = ""

    line match {
      case State.Header(_) => fail()
      case State.Action(_) => fail()
      case State.Next(_) => fail()
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
      State.Header.unapply(line),
      State.Action.unapply(line),
      State.Next.unapply(line)
    )).foreach(list =>
      assert(list.count(_.isDefined) == 1)
    )
  }

  it should "parse empty State" in {
    val lines = List(
      "State 123",
      "",
      "",
      ""
    )

    val state = State(lines)
    assert(state == State(
      length = 1,
      next = None,
      action = None
    ))
  }
}
