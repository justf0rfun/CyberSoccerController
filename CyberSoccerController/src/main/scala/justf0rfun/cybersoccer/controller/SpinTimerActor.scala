package justf0rfun.cybersoccer.controller

class SpinTimerActor(repetitionTimeInterval: Long, f: Any => Unit) extends Thread {

	private var running = true
	private var lastExecutionInterval: Long = executionTimeInterval

	private def executionTimeInterval: Long = System.nanoTime() / repetitionTimeInterval

	override def start(): Unit = {
		running = true
		super.start()
	}
	
	override def run() = {
		spin
	}

	private def spin: Unit = {
		while (running) {
			if (executionTimeInterval != lastExecutionInterval) {
				lastExecutionInterval = executionTimeInterval
				f.apply()
			}
		}
	}
	
	def isRunngin = running

	def stopTimer(): Unit = running = false

}