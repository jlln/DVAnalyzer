
object scratchpad {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(43); 
	val t = List("a","b");System.out.println("""t  : List[String] = """ + $show(t ));$skip(14); 
	val ta = "c";System.out.println("""ta  : String = """ + $show(ta ));$skip(9); val res$0 = 
	ta +: t;System.out.println("""res0: List[String] = """ + $show(res$0))}
}
