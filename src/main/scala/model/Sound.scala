package model

case class Sound(
  id: Int,
  singular: Option[Int] = None,
  priority: Option[Int] = None,
  flags: Option[Int] = None
)
