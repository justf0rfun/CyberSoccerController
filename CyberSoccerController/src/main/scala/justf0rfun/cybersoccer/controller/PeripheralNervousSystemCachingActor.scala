package justf0rfun.cybersoccer.controller
import akka.actor.ActorRef
import akka.actor.Actor
import justf0rfun.cybersoccer.model.SoccerField
import justf0rfun.cybersoccer.model.MatchState
import justf0rfun.cybersoccer.model.Body
import justf0rfun.cybersoccer.centralnervoussystems.PeripheralNervousSystem
import justf0rfun.cybersoccer.centralnervoussystems.CentralNervousSystemFactory
import justf0rfun.cybersoccer.centralnervoussystems.CentralNervousSystem
import justf0rfun.cybersoccer.model.Goal

class PeripheralNervousSystemCachingActor(universe: ActorRef, val field: SoccerField, centralNervousSystemFactory: CentralNervousSystemFactory) extends Actor with PeripheralNervousSystem{

  private val centralNervousSystem: CentralNervousSystem = centralNervousSystemFactory.createCentralNervousSystem(this)

  var matchState: MatchState = null
  var body: Body = null
  var targetGoal: Goal = null

  //  def shout(message: Any): Unit = universe ! SoccerProtocol.Shouting(message)

  def move(velocity: Double, angle: Double) = universe ! (velocity: Double, angle: Double)

  def receive = {
    case (newBody: Body, newMatchState: MatchState) => {
      body = newBody
      matchState = newMatchState
      centralNervousSystem.think()
    }
  }

}