package justf0rfun.cybersoccer.controller
import justf0rfun.cybersoccer.model.Ball
import justf0rfun.cybersoccer.model.Body
import justf0rfun.cybersoccer.model.Kick

class BodyBallDistanceKickOrdering(ball: Ball) extends ExtendedOrdering[Tuple2[Body, Kick]] {

	override def compare(tupleA: Tuple2[Body, Kick], tupleB: Tuple2[Body, Kick]) = {
		(tupleA._1.location.distance(ball.location) - tupleB._1.location.distance(ball.location)).toInt
	}

}