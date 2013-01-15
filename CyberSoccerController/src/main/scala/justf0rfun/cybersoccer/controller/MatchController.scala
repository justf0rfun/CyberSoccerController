package justf0rfun.cybersoccer.controller

import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.math._
import scala.util.Random
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import akka.actor.actorRef2Scala
import justf0rfun.cybersoccer.centralnervoussystems.CentralNervousSystemFactory
import justf0rfun.cybersoccer.model.Ball
import justf0rfun.cybersoccer.model.Body
import justf0rfun.cybersoccer.model.Goal
import justf0rfun.cybersoccer.model.Goal
import justf0rfun.cybersoccer.model.Kick
import justf0rfun.cybersoccer.model.MatchConfiguration
import justf0rfun.cybersoccer.model.MatchState
import justf0rfun.cybersoccer.model.Move
import justf0rfun.cybersoccer.model.RefereeDecision
import justf0rfun.cybersoccer.model.Team
import justf0rfun.mathematics.geometry.linear.LineSegment
import justf0rfun.mathematics.geometry.linear.Vector
import justf0rfun.mathematics.geometry.Angle
import justf0rfun.mathematics.geometry.Point

private object NextStep

class MatchController(hostCentralNervousSystemFactory: CentralNervousSystemFactory, guestCentralNervousSystemFactory: CentralNervousSystemFactory, matchConfiguration: MatchConfiguration, val cycleTimeInterval: Long) extends Actor {

	private val subscribers = Set[ActorRef]()
	private val playerActions: Map[ActorRef, Any] = Map()
	private val playerBodies: Map[ActorRef, Body] = Map()
	private var matchState: MatchState = null
	private lazy val referee: ActorRef = context.actorOf(Props(new Referee(self, matchConfiguration.duration)), name = "referee")
	private var latestRefereeDecision: Option[RefereeDecision] = None
	private var matchStateTimerIntervalMeasurement: TimeMeasurement = null
	private val scheduler = new SleepScheduler(self ! NextStep, cycleTimeInterval)

	override def receive = {
		case NextStep => {
			if (matchState == null) {
				startMatch
			} else {
				matchState = nextState(matchState, playerActions)
				updateBodies
				publishMatchState(matchState)
//				Thread.sleep(TimeMeasurement.nanoSecondsToMilliSeconds(cycleTimeInterval))
//				self ! NextStep
			}
		}
		case move: Move => {
			playerActions += (sender -> move)
		}
		case kick: Kick => {
			playerActions += (sender -> kick)
		}
		case PublishAndSubscribeProtocol.Subscribe(subscriber: ActorRef) => {
			subscribers += subscriber
			subscriber ! matchState
		}
		case PublishAndSubscribeProtocol.Unsubscribe(subscriber) => subscribers -= subscriber
		case PublishAndSubscribeProtocol.RequestLatestPublication => sender ! PublishAndSubscribeProtocol.Publication(matchState)
		case refereeDecision: RefereeDecision if (sender == referee) => {
			//Maybe just tops myself
			latestRefereeDecision = Some(refereeDecision)
			publishRefereeDecision(refereeDecision)
		}
	}

	private def startMatch = {
		subscribers += referee
		matchState = initializeMatch
		updateBodies
		publishMatchState(matchState)
		self ! NextStep
	}

	private def initializeMatch = {
		val host: Team = new Team(hostCentralNervousSystemFactory.teamName)
		val guest: Team = new Team(guestCentralNervousSystemFactory.teamName)
		val ball: Ball = createKickOffBall
		val initialLocation = matchConfiguration.field.bottomLine.middle
		for (i <- 0 until matchConfiguration.numberOfPlayers) {
			playerBodies += context.actorOf(Props(new PeripheralNervousSystemCachingActor(hostCentralNervousSystemFactory, self)), name = "hostPlayer_%d".format(i)) -> new Body(host, i, initialLocation, new Move(Vector.zeroVector), matchConfiguration.playerRangeRadius, matchConfiguration.velocityMaximum, matchConfiguration.kickForceMaximum, Nil)
			playerBodies += context.actorOf(Props(new PeripheralNervousSystemCachingActor(guestCentralNervousSystemFactory, self)), name = "guestPlayer_%d".format(i)) -> new Body(guest, i, initialLocation, new Move(Vector.zeroVector), matchConfiguration.playerRangeRadius, matchConfiguration.velocityMaximum, matchConfiguration.kickForceMaximum, Nil)
		}
		subscribers ++= playerBodies.keys
		matchStateTimerIntervalMeasurement = new TimeMeasurement()
		new MatchState(matchConfiguration.field, ball, playerBodies.values, host, guest, matchConfiguration.field.rightGoal, matchConfiguration.field.leftGoal, None)
	}

	//	private def createPlayerActor(team: Team, index: Int) = context.actorOf(Props(new PeripheralNervousSystemCachingActor(hostCentralNervousSystemFactory, self)), name = "hostPlayer_%d".format(index))

	private def nextState(matchState: MatchState, playerActions: Map[ActorRef, Any]) = {
		val timeIntervalSinceLastMatchState = matchStateTimerIntervalMeasurement.elapsedTime
		var newBall: Ball = matchState.ball
		//kick
		var kicks = playerActions.collect(e => e match {
			case (peripheralNervousSystem: ActorRef, kick: Kick) if (playerBodies(peripheralNervousSystem).isBallInRange(matchState.ball)) => {
				if (kick.vector.distance <= matchConfiguration.kickForceMaximum) {
					(playerBodies(peripheralNervousSystem), kick)
				} else {
					(playerBodies(peripheralNervousSystem), new Kick(new Vector(kick.vector.angle, matchConfiguration.kickForceMaximum)))
				}
			}
//			case _ =>
		})
		var kickingBody: Option[Body] = None
		if (!kicks.isEmpty) {
			val kickingBodyAndKick = new BodyBallDistanceKickOrdering(matchState.ball).min(kicks) match {
				case kicks => kicks.size match {
					case 1 => kicks.head
					case _ => kicks(Random.nextInt(kicks.size))
				}
			}
			//			newBall = kickBall(matchState.ball, kickingBodyAndKick._1, kickingBodyAndKick._2, timeIntervalSinceLastMatchState)
			newBall = matchState.ball.kick(kickingBodyAndKick._2, timeIntervalSinceLastMatchState)
			kickingBody = Some(kickingBodyAndKick._1)
		}
		//move bodies
		playerActions.foreach({
			case (peripheralNervousSystem: ActorRef, move: Move) => playerBodies += (peripheralNervousSystem -> playerBodies(peripheralNervousSystem).move(move, timeIntervalSinceLastMatchState))
			case _ =>
		})
		playerActions.clear()
		latestRefereeDecision match {
			case Some(RefereeDecision.Goal(team)) => newBall = createKickOffBall
			case Some(RefereeDecision.GoalKick(defendingTeam)) => newBall = createKickOffBall
			case Some(RefereeDecision.CornerKick(team, point)) => newBall = createSetPieceBall(point)
			case Some(RefereeDecision.ThrowIn(team, point)) => newBall = createSetPieceBall(point)
			case Some(RefereeDecision.Finish(winner)) => {
				playerBodies.keys.foreach(context.stop)
				context.stop(referee)
			}
			case _ => newBall = newBall.move(timeIntervalSinceLastMatchState)
		}
		latestRefereeDecision = None

		matchStateTimerIntervalMeasurement = new TimeMeasurement()
		new MatchState(matchState.field, newBall, playerBodies.values.toSet, matchState.host, matchState.guest, matchState.hostTargetGoal, matchState.guestTargetGoal, kickingBody)
	}

	private def teamByTargetGoal(targetGoal: Goal): Team = if (targetGoal == matchState.hostTargetGoal) matchState.host else matchState.guest

	private def updateBodies = {
		playerBodies.foreach {
			case (peripheralNervousSystem: ActorRef, body: Body) => peripheralNervousSystem ! body
		}
	}

	private def publishMatchState(matchState: MatchState) = {
		val update = PublishAndSubscribeProtocol.Publication(matchState)
		subscribers.foreach { _ ! update }
	}

	private def publishRefereeDecision(refereeDecision: RefereeDecision) = {
		val update = PublishAndSubscribeProtocol.Publication(refereeDecision)
		subscribers.foreach { _ ! update }
	}

	private def createKickOffBall = createSetPieceBall(matchConfiguration.field.kickOffPoint.location);

	private def createSetPieceBall(location: Point) = new Ball(location, matchConfiguration.ballRadius, new Move(Vector.zeroVector), matchConfiguration.ballFriction);

}

object MatchController {

	def startMatch(matchController: ActorRef) = matchController ! NextStep

}