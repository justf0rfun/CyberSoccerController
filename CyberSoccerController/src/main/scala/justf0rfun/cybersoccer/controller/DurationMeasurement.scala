package justf0rfun.cybersoccer.controller

class DurationMeasurement(val duration: Long) extends TimeMeasurement {

	def remainingTime = duration - elapsedTime
		
	def isFinished = remainingTime <= 0

	def finish = begin + duration

}