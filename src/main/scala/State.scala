case class State(
  length: Int,
  next: Option[Int],
  action: Option[String]
)

object State extends Entry[State] {
  object BlockHeader extends Entry.Header("State")
  object Next extends Entry.Property[Int]("NEXT")
  object Action extends Entry.Property.SpaceDelimited[String]("ACTION")

//  def readState(lines: Seq[String]): State = {
//    val header = lines.headOption
//    val body = lines.tail.takeWhile(!Entry.isBlank(_))
//
//    val init = State(
//      length = header.size + body.size,
//      next = None,
//      action = None
//    )
//
//    body.foldLeft(init)((state, line) => {
//      line match {
//        case Next(nextStateId) => state.copy(next = Some(nextStateId))
//        case Action(name) => state.copy(action = Some(name))
//        case _ => {
//          println(s"Warning: couldn't parse line: $line")
//          state
//        }
//      }
//    })
//  }

  //def apply(lines: Seq[String]): State = readState(lines)
  override def initialState(contents: Seq[String]): State = State(
    length = contents.size,
    next = None,
    action = None
  )

  override def parse(state: State, line: String): State = {
    line match {
      case Next(nextStateId) => state.copy(next = Some(nextStateId))
      case Action(name) => state.copy(action = Some(name))
      case _ => {
        println(s"Warning: couldn't parse line: $line")
        state
      }
    }
  }
}