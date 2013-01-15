package justf0rfun.cybersoccer.controller
import scala.collection.immutable.Queue
import akka.actor.Actor
import akka.actor.ActorRef
import justf0rfun.cybersoccer.model.Body
import justf0rfun.cybersoccer.model.Goal
import justf0rfun.cybersoccer.model.Kick
import justf0rfun.cybersoccer.model.MatchConfiguration
import justf0rfun.cybersoccer.model.MatchState
import justf0rfun.cybersoccer.model.Move
import justf0rfun.cybersoccer.model.SoccerField
import justf0rfun.cybersoccer.centralnervoussystems.PeripheralNervousSystem
import justf0rfun.cybersoccer.centralnervoussystems.CentralNervousSystemFactory
import justf0rfun.cybersoccer.centralnervoussystems.CentralNervousSystem
import justf0rfun.cybersoccer.model.RefereeDecision

class PeripheralNervousSystemCachingActor(centralNervousSystemFactory: CentralNervousSystemFactory, matchController: ActorRef) extends Actor with PeripheralNervousSystem {

	private val centralNervousSystem: CentralNervousSystem = centralNervousSystemFactory.createCentralNervousSystem(this)
	private var refereeDecisionQueue = Queue[RefereeDecision]()

	var matchState: MatchState = null
	var body: Body = null

	//  def shout(message: Any): Unit = universe ! SoccerProtocol.Shouting(message)

	override def move(move: Move) = matchController ! move

	override def kickBall(kick: Kick) = matchController ! kick

	override def receive = {
		case newBody: Body => body = newBody
		case PublishAndSubscribeProtocol.Publication(newMatchState: MatchState) => {
			matchState = newMatchState
			centralNervousSystem.think()
		}
		case PublishAndSubscribeProtocol.Publication(e: RefereeDecision) => refereeDecisionQueue = refereeDecisionQueue.enqueue(e)
	}

	override def nextRefereeDecision: Option[RefereeDecision] = {
		if (refereeDecisionQueue.isEmpty) {
			return None
		}
		val tmpTuple = refereeDecisionQueue.dequeue
		refereeDecisionQueue = tmpTuple._2
		return Some(tmpTuple._1)
	}

}