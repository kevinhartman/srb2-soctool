package model

case class State(
  id: Int,
  spriteNumber: Option[Int] = None,
  spriteSubNumber: Option[Int] = None,
  duration: Option[Int] = None,
  next: Option[Int] = None,
  action: Option[String] = None,
  var1: Option[Int] = None,
  var2: Option[Int] = None
)
