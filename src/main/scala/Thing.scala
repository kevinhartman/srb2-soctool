case class Thing(
  length: Int
)

object Thing extends Entry[Thing] {
  object BlockHeader extends Entry.Header("Thing")
  object SpawnState extends Entry.Property[Int]("SPAWNSTATE")
  object SeeState extends Entry.Property[Int]("SEESTATE")
  object PainState extends Entry.Property[Int]("PAINSTATE")
  object MeleeState extends Entry.Property[Int]("MELEESTATE")
  object MissileState extends Entry.Property[Int]("MISSILESTATE")
  object DeathState extends Entry.Property[Int]("DEATHSTATE")
  object XDeathState extends Entry.Property[Int]("XDEATHSTATE")
  object RaiseState extends Entry.Property[Int]("RAISESTATE")

//  def readThing(lines: Seq[String]): Thing = {
//    val header = lines.headOption
//    val body = lines.tail.takeWhile(!Entry.isBlank(_))
//
//    val init = Thing(
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
  override def initialState(contents: Seq[String]): Thing = Thing(
    length = contents.size
  )

  override def parse(current: Thing, line: String): Thing = ???
}
