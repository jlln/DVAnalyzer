
object scratchpad {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(43); 
	val t = List(1,2,3,4);System.out.println("""t  : List[Int] = """ + $show(t ));$skip(23); 
	val m = Stats.mean(t);System.out.println("""m  : Double = """ + $show(m ));$skip(23); 
	val d = t.map(x=>x-m);System.out.println("""d  : List[Double] = """ + $show(d ));$skip(44); 
	val s2 = d.map(x=>scala.math.pow(x,2)).sum;System.out.println("""s2  : Double = """ + $show(s2 ));$skip(45); 
	val s4a = d.map(x=>scala.math.pow(x,4)).sum;System.out.println("""s4a  : Double = """ + $show(s4a ));$skip(32); 
	val s4b = scala.math.pow(s2,2);System.out.println("""s4b  : Double = """ + $show(s4b ))}
}
