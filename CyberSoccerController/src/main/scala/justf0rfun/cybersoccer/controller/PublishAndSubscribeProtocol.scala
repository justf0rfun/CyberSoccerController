package justf0rfun.cybersoccer.controller
import akka.actor.ActorRef

object PublishAndSubscribeProtocol {

  sealed trait PublishAndSubscribeProtocolMessage
  case class Subscribe(subscriber: ActorRef) extends PublishAndSubscribeProtocolMessage
  case class Unsubscribe(subscriber: ActorRef) extends PublishAndSubscribeProtocolMessage
  case class Publication(publication: Any) extends PublishAndSubscribeProtocolMessage
  case object RequestLatestPublication extends PublishAndSubscribeProtocolMessage

}