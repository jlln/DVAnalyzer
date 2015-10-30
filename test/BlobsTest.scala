

/**
 * @author james
 */
import org.scalatest._

class BlobSuite extends FunSuite with Matchers{



val test_pixel_array = Array(
    Array(-1,-1,0,-1,-1),
    Array(-1,-1,0,-1,-1),
    Array( 0, 0,0, 0, 0),
    Array( 0, 0,0,-1,-1),
    Array( 0, 0,0,-1,-1))
    
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




val test_image_1_path = "/home/james/workspace/Zisa/test/test_image_1.tif"
val test_image_1 = ImageIO.openImageFile(test_image_1_path)
val (nuclei_1,channels_1,mask) = NucleiProcessing.processImageToNuclei(test_image_1)
val red_channel_n1:Array[Array[Array[Int]]] = ObjectThresholding.thresholdObjects(nuclei_1(0), channels_1(0))._1
val blue_channel_n1:Array[Array[Array[Int]]] = ObjectThresholding.thresholdObjects(nuclei_1(0), channels_1(2))._1
test("blob stats"){
  val blob_stats = Blobs.analyzePixelArrayStack(blue_channel_n1,"Blue",nuclei_1.head.getCentroids)
   (blob_stats.getEntries.length === 13)

}

}