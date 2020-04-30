package model

case class State(
  id: String,
  spriteNumber: Option[String] = None,
  spriteSubNumber: Option[String] = None,
  duration: Option[String] = None,
  next: Option[String] = None,
  action: Option[String] = None,
  var1: Option[String] = None,
  var2: Option[String] = None
)
