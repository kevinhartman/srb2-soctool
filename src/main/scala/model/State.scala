package model

case class State(
  id: Int,
  next: Option[Int] = None,
  action: Option[String] = None
)
