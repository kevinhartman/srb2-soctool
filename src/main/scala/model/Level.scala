package model

case class Level(
  id: String,
  levelName: Option[String] = None,
  nextLevel: Option[String] = None,
  musicSlot: Option[String] = None,
  weather: Option[String] = None,
  skyNum: Option[String] = None,
  noZone: Option[String] = None,
  noSSMusic: Option[String] = None,
  levelSelect: Option[String] = None,
  typeOfLevel: Option[String] = None
)
