
object scratchpad {
	val t = List("a","b")                     //> t  : List[String] = List(a, b)
	val ta = "c"                              //> ta  : String = c
	ta +: t                                   //> res0: List[String] = List(c, a, b)
}