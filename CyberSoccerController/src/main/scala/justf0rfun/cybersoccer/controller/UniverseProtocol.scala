package justf0rfun.cybersoccer.controller

object UniverseProtocol {

  sealed trait UniverseProtocolMessage
  case object Subscribe extends UniverseProtocolMessage
  case object Unsubscribe extends UniverseProtocolMessage
  case class Update(update: Any) extends UniverseProtocolMessage

}