
object scratchpad {
	val test_pixel_array_2 = Array(
    Array(0,0,0,0,0,0,0,0),
    Array(0,1,1,1,1,1,0,0),
    Array(0,1,0,0,0,1,0,0),
    Array(0,1,0,0,0,1,0,0),
    Array(0,1,0,0,0,1,0,0),
    Array(0,1,0,0,0,1,0,0),
    Array(0,1,1,1,1,1,0,0),
    Array(0,0,0,0,0,0,0,0)
    
  )
  val label_array:Array[Array[Int]] = Array.ofDim[Int](8,8)
  Blobs.contourTrace(test_pixel_array_2, label_array,1, 1, 7, 8, 8, 1)
}