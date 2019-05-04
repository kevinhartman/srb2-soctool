package model

case class Sound(
  id: Int,
  priority: Option[Int] = None,
  flags: Option[Int] = None
)
