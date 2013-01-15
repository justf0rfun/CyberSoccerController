package justf0rfun.cybersoccer.controller

import akka.actor.Actor
import akka.actor.ActorRef
import justf0rfun.cybersoccer.model.Ball
import justf0rfun.cybersoccer.model.Body
import justf0rfun.cybersoccer.model.MatchState
import justf0rfun.cybersoccer.model.RefereeDecision
import justf0rfun.cybersoccer.model.SoccerField
import justf0rfun.cybersoccer.model.Team
import justf0rfun.mathematics.geometry.linear.LineSegment
import justf0rfun.mathematics.geometry.Point

class Referee(matchController: ActorRef, matchDuration: Long) extends Actor {

	private var precedingMatchState: MatchState = null
	private var lastBallCrossedLineSegment: Option[(LineSegment, Point)] = None
	private var lastKickingBody: Option[Body] = None
	private var hostScore: Int = 0
	private var guestScore: Int = 0
	private var lastDecision: Option[RefereeDecision] = None
	private lazy val durationMeasurement = new DurationMeasurement(matchDuration)

	override def receive = {
		case PublishAndSubscribeProtocol.Publication(matchState: MatchState) if (sender == matchController) =>
			{
				matchState.kickingBody match {
					case Some(kicker) => lastKickingBody = Some(kicker)
					case _ =>
				}
				if (durationMeasurement.isFinished) {
					matchController ! RefereeDecision.Finish(leadingTeam)
				} else if (precedingMatchState != null) {
					val ballMovementVector = new LineSegment(precedingMatchState.ball.location, matchState.ball.location)
					crossedLineSegmentAndIntersectionPoint(matchState.field, ballMovementVector) match {
						case Some(crossedLineSegmentAndIntersectionPoint: (LineSegment, Point)) => {
							lastBallCrossedLineSegment = Some(crossedLineSegmentAndIntersectionPoint)
						}
						case None =>
					}
					if (lastBallCrossedLineSegment != None && matchState.field.isOutsideField(matchState.ball)) {
						val decision: Option[RefereeDecision] = lastBallCrossedLineSegment match {
							case Some((lineSegment, intersectionPoint)) if (lineSegment == matchState.hostTargetGoal.goalLine) => {
								hostScore += 1
								Some(RefereeDecision.Goal(matchState.host))
							}
							case Some((lineSegment, intersectionPoint)) if (lineSegment == matchState.guestTargetGoal.goalLine) => {
								guestScore += 1
								Some(RefereeDecision.Goal(matchState.guest))
							}
							case Some((lineSegment, intersectionPoint)) if (lineSegment == matchState.field.leftLine) => {
								val defendingTeam = matchState.leftTeam
								lastKickingBody match {
									case Some(lastKickingBody) if (lastKickingBody.team == matchState.oponent(defendingTeam)) => Some(RefereeDecision.GoalKick(defendingTeam))
									case _ => Some(RefereeDecision.CornerKick(matchState.oponent(defendingTeam), matchState.field.kickOffPoint.location))
								}
							}
							case Some((lineSegment, intersectionPoint)) if (lineSegment == matchState.field.rightLine) => {
								val defendingTeam = matchState.rightTeam
								lastKickingBody match {
									case Some(lastKickingBody) if (lastKickingBody.team == matchState.oponent(defendingTeam)) => Some(RefereeDecision.GoalKick(defendingTeam))
									case _ => Some(RefereeDecision.CornerKick(matchState.oponent(defendingTeam), matchState.field.kickOffPoint.location))
								}
							}
							case Some((lineSegment, intersectionPoint)) if (lineSegment == matchState.field.upperLine) => {
								lastKickingBody match {
									case Some(lastKickingBody) => {
										val oponent = matchState.oponent(lastKickingBody.team)
										Some(RefereeDecision.ThrowIn(oponent, matchState.ball.location))
									}
									case None => None
								}
							}
							case Some((lineSegment, intersectionPoint)) if (lineSegment == matchState.field.bottomLine) => {
								lastKickingBody match {
									case Some(lastKickingBody) => {
										val oponent = matchState.oponent(lastKickingBody.team)
										Some(RefereeDecision.ThrowIn(oponent, matchState.ball.location))
									}
									case None => None
								}
							}
						}
						decision match {
							case Some(decision) => {
								lastDecision = Some(decision)
								matchController ! decision
								lastBallCrossedLineSegment = None
							}
							case None =>
						}
					}
				} else {
					matchController ! RefereeDecision.KickOff(matchState.host)
				}
			}
			precedingMatchState = matchState
	}

	private def leadingTeam: Option[Team] = if (hostScore < guestScore) Some(precedingMatchState.guest) else if (guestScore < hostScore) Some(precedingMatchState.host) else None

	private def crossedLineSegmentAndIntersectionPoint(field: SoccerField, movementVector: LineSegment): Option[(LineSegment, Point)] = {
		val lines = List(field.leftGoal.goalLine, field.rightGoal.goalLine, field.leftLine, field.rightLine, field.upperLine, field.bottomLine)
		var intersectionPoint: Point = null
		return lines.find(lineSegment => movementVector.intersectionPoint(lineSegment) match {
			case Some(point) => {
				intersectionPoint = point
				true
			}
			case None => false
		}) match {
			case Some(crossedLineSegment) => Some((crossedLineSegment, intersectionPoint))
			case None => None
		}
		return None
	}

	//			private def crossedLineSegmentAndIntersectionPoint(field: SoccerField, movementVector: LineSegment): Option[(LineSegment, Point)] = {
	////		val lines = List(field.leftGoal.goalLine, field.rightGoal.goalLine, field.leftLine, field.rightLine, field.upperLine, field.bottomLine)
	////		var intersectionPoint: Point = null
	////		lines.find(lineSegment => movementVector.intersectionPoint(lineSegment) match {
	////			case Some(point) => {
	////				intersectionPoint = point
	////				true
	////			}
	////			case None => false
	//////					lines.find(lineSegment => movementVector.intersectionPoint(lineSegment) match {
	//////					case Some(point) => {
	//////						intersectionPoint = point
	//////								true
	//////					}
	//////					case None => false
	////		}) match {
	////			case Some(crossedLineSegment) => Some((crossedLineSegment, intersectionPoint))
	////			case None => None
	////		}
	//			return None
	//	}

	//	private def ballLeavingFieldTimeInterval(ball: Ball, field: SoccerField) = 
	//	private def ballLeavingFieldTimeInterval(ball: Ball, field: SoccerField) = 

}