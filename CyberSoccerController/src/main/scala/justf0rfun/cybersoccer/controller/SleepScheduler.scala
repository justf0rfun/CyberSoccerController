package justf0rfun.cybersoccer.controller

class SleepScheduler(task: => Unit, timeInterval: Long) {

	var isRunning = false
	val thread = new Thread(new Runnable {

		override def run() {
			isRunning = true
			while (isRunning) {
				Thread.sleep(TimeMeasurement.nanoSecondsToMilliSeconds(timeInterval))
				task
			}
		}
	}, "SchedulerThread")

	thread.start()

}