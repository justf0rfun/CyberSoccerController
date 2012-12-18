package justf0rfun.cybersoccer.controller
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import justf0rfun.cybersoccer.model.Ball
import justf0rfun.cybersoccer.model.MatchState
import justf0rfun.cybersoccer.model.SoccerField
import justf0rfun.cybersoccer.model.Team
import justf0rfun.cybersoccer.centralnervoussystems.CentralNervousSystemFactory
import justf0rfun.cybersoccer.model.SoccerEvents
import justf0rfun.cybersoccer.model.Body
import justf0rfun.mathematics.geometry.Point

private object NextStep

abstract class Universe(hostCentralNervousSystemFactory: CentralNervousSystemFactory, guestCentralNervousSystemFactory: CentralNervousSystemFactory, numberOfPlayers: Int, timerInterval: Double = 100, duration: Long, maximumVelocity: Double, goalWidth: Double) extends Actor {
  
  case class Shouting(message: Any)
  private val host: Team = new Team("host", 0)
  private val guest: Team = new Team("guest", 0)
  private var ball: Ball = new Ball(Point(0, 0), 0, 0, 4)
  private var events = List[SoccerEvents]()
  private var time: Long = 0
  private var playerBodies: Map[ActorRef, Body] = Map()
  private var field = new SoccerField(100, 200, goalWidth) // propagate
  for (i <- 0 until numberOfPlayers) {
    playerBodies += context.actorOf(Props(new PeripheralNervousSystemCachingActor(self, field, hostCentralNervousSystemFactory))) -> new Body(Point(0, 0), 0, 0, 3, host, Nil)
    playerBodies += context.actorOf(Props(new PeripheralNervousSystemCachingActor(self, field, guestCentralNervousSystemFactory))) -> new Body(Point(0, 0), 0, 0, 3, guest, Nil)
  }
  private var matchState = new MatchState(ball, playerBodies.values, events, host, guest)
  private val remainingPlayers: Set[ActorRef] = Set[ActorRef]()
  private var begin = System.currentTimeMillis
  private val subscribers = Set[ActorRef]()
  notifyPlayers
  private var playerActions: Map[ActorRef, (Double, Double)] = Map()

  //  context.system.scheduler.schedule(Duration.Zero, Duration.create(timerInterval, TimeUnit.MILLISECONDS), self, NextStep)

  override def receive = {
    case NextStep => {
      if (isMatchFinished) {
        //Maybe just tops myself
        playerBodies.keys.foreach(context.stop)
      } else {
        nextState
        notifyPlayers
      }
    }
    //    case shouting: Shouting => sender ! shouting // TODO
    case action: (Double, Double) => {
      playerActions += (sender -> action)
      remainingPlayers -= sender // reassignment may be necessarry
      if (remainingPlayers.isEmpty) self ! NextStep
    }
    case UniverseProtocol.Subscribe => subscribers += sender
    case UniverseProtocol.Unsubscribe => subscribers -= sender
  }

  private def nextState = {
    val newBall = null
    playerActions.filter(entry => entry._2._1.abs <= maximumVelocity && entry._2._2.abs <= maximumVelocity)
    matchState = new MatchState(newBall, playerBodies.values, events, host, guest)
  }

  private def remainingTime = System.currentTimeMillis() - begin + duration

  private def isMatchFinished = remainingTime < 1

  private def notifyPlayers = {
    remainingPlayers.clear()
    playerBodies.foreach {
      case (peripheralNervousSystem: ActorRef, body: Body) => {
        remainingPlayers += peripheralNervousSystem
        peripheralNervousSystem ! ((playerBodies(_), matchState))
      }
    }
  }
  def publishUpdate = {
    val update = UniverseProtocol.Update(matchState)
    subscribers.foreach { _ ! update }
  }

}