
object scratchpad {
	val t = List(1,2,3,4)                     //> t  : List[Int] = List(1, 2, 3, 4)
	val m = Stats.mean(t)                     //> m  : Double = 2.5
	val d = t.map(x=>x-m)                     //> d  : List[Double] = List(-1.5, -0.5, 0.5, 1.5)
	val s2 = d.map(x=>scala.math.pow(x,2)).sum//> s2  : Double = 5.0
	val s4a = d.map(x=>scala.math.pow(x,4)).sum
                                                  //> s4a  : Double = 10.25
	val s4b = scala.math.pow(s2,2)            //> s4b  : Double = 25.0
}