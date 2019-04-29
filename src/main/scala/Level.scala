case class Level(
  length: Int
)

object Level {
  object BlockHeader extends Entry.Header("Level")
}