

/**
 * @author james
 */
import ij.ImagePlus
import org.scalatest._
class ThresholdSuite extends FunSuite with Matchers{
  val test_image_1_path = "/home/james/workspace/Zisa/test/test_image_1.tif"
  val test_image_1 = ImageIO.openImageFile(test_image_1_path)
  val (nuclei_1,channels_1,mask) = NucleiProcessing.processImageToNuclei(test_image_1)
  val red_channel_n1:Array[Array[Array[Int]]] = ObjectThresholding.thresholdObjects(nuclei_1(0), channels_1(0))._1
  val red_channel_n2:Array[Array[Array[Int]]] = ObjectThresholding.thresholdObjects(nuclei_1(1), channels_1(0))._1
  val red_channel_n3:Array[Array[Array[Int]]] = ObjectThresholding.thresholdObjects(nuclei_1(2), channels_1(0))._1
  val red_channel_n4:Array[Array[Array[Int]]] = ObjectThresholding.thresholdObjects(nuclei_1(3), channels_1(0))._1
  val blue_channel_n1:Array[Array[Array[Int]]] = ObjectThresholding.thresholdObjects(nuclei_1(0), channels_1(2))._1
  val green_channel_n1:Array[Array[Array[Int]]] = ObjectThresholding.thresholdObjects(nuclei_1(0), channels_1(1))._1
  test("Thresholding"){
    
    val flat_rcn1:Array[Int] = red_channel_n1.flatten.flatten
    flat_rcn1.max should equal (255)
    flat_rcn1.min should equal(0)
  }
  test("VisualCheck"){
    List(red_channel_n1,red_channel_n2,red_channel_n3,red_channel_n4,blue_channel_n1,green_channel_n1).map{ n=>
      n.map{
        s=>ImageIO.drawPixels(ImageIO.makeImage(s))
      }
    }
    
  }
}