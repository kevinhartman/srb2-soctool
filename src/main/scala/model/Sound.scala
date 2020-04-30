package model

case class Sound(
  id: String,
  singular: Option[String] = None,
  priority: Option[String] = None,
  flags: Option[String] = None
)
