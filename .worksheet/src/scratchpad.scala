
object scratchpad {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(285); 
	val test_pixel_array_2 = Array(
    Array(0,0,0,0,0,0,0,0),
    Array(0,1,1,1,1,1,0,0),
    Array(0,1,0,0,0,1,0,0),
    Array(0,1,0,0,0,1,0,0),
    Array(0,1,0,0,0,1,0,0),
    Array(0,1,0,0,0,1,0,0),
    Array(0,1,1,1,1,1,0,0),
    Array(0,0,0,0,0,0,0,0)
    
  );System.out.println("""test_pixel_array_2  : Array[Array[Int]] = """ + $show(test_pixel_array_2 ));$skip(60); 
  val label_array:Array[Array[Int]] = Array.ofDim[Int](8,8);System.out.println("""label_array  : Array[Array[Int]] = """ + $show(label_array ));$skip(71); val res$0 = 
  Blobs.contourTrace(test_pixel_array_2, label_array,1, 1, 7, 8, 8, 1);System.out.println("""res0: Array[Array[Int]] = """ + $show(res$0))}
}
