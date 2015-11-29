
object scratchpad {
	val a = List(List(1,2),List(3,4),List(5,6))
                                                  //> a  : List[List[Int]] = List(List(1, 2), List(3, 4), List(5, 6))
	
	val b = List(List(1,2),List(3,4),List(5,6))
                                                  //> b  : List[List[Int]] = List(List(1, 2), List(3, 4), List(5, 6))
	
	a == b                                    //> res0: Boolean = true
  }