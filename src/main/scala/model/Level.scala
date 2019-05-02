package model

case class Level(
  id: Int,
  nextLevel: Option[Int] = None
)
