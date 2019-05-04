package model

case class Level(
  id: Int,
  levelName: Option[String] = None,
  nextLevel: Option[Int] = None,
  musicSlot: Option[Int] = None,
  weather: Option[Int] = None,
  skyNum: Option[Int] = None,
  noZone: Option[Int] = None,
  noSSMusic: Option[Int] = None,
  levelSelect: Option[Int] = None,
  typeOfLevel: Option[Int] = None
)
