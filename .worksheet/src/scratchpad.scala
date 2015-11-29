
object scratchpad {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(65); 
	val a = List(List(1,2),List(3,4),List(5,6));System.out.println("""a  : List[List[Int]] = """ + $show(a ));$skip(47); 
	
	val b = List(List(1,2),List(3,4),List(5,6));System.out.println("""b  : List[List[Int]] = """ + $show(b ));$skip(10); val res$0 = 
	
	a == b;System.out.println("""res0: Boolean = """ + $show(res$0))}
  }
