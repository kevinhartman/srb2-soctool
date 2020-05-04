import model._

case class PrinterConfig(
  levelFilter: Entry[Level] => Boolean = _ => true,
  objectFilter: Entry[Object] => Boolean = _ => true,
  stateFilter: Entry[State] => Boolean = _ => true,
  soundFilter: Entry[Sound] => Boolean = _ => true
)