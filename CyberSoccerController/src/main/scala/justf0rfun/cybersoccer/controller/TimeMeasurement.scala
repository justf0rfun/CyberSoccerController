package justf0rfun.cybersoccer.controller

class TimeMeasurement(val begin: Long = System.nanoTime()) {

	def elapsedTime = System.nanoTime() - begin

}

object TimeMeasurement {

	private val nanoToMilliFactor = 1000000l

	def nanoSecondsToMilliSeconds(nanoSeconds: Long) = nanoSeconds / nanoToMilliFactor

	def milliSecondsToNanoSeconds(milliSeconds: Long) = milliSeconds * nanoToMilliFactor
}