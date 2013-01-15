package justf0rfun.cybersoccer.controller

//TODO this trait is a copy from MyCentralNervousSystem and should be outsourced to a single location for reuse
trait ExtendedOrdering[T] extends Ordering[T] {

	def max(iterable: Iterable[T]): Seq[T] = {
		val iterator = iterable.iterator
		var maxList = List(iterator.next())
		iterator.foreach(
			e => compare(maxList.head, e) match {
				case i if (i < 0) =>
				case i if (i == 0) => maxList = e :: maxList
				case i if (0 < i) => maxList = List(e)
			})
		return maxList
	}

	def min(iterable: Iterable[T]): Seq[T] = {
		val iterator = iterable.iterator
		var minList = List(iterator.next())
		iterator.foreach(
			e => compare(minList.head, e) match {
				case i if (i < 0) => minList = List(e)
				case i if (i == 0) => minList = e :: minList
				case i if (0 < i) => 
			})
		return minList
	}

}