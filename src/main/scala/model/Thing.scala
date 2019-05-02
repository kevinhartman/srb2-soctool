package model

case class Thing(
  id: Int,
  states: Map[String, Int] = Map(),
  sounds: Map[String, Int] = Map()
)
