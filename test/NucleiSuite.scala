

/**
 * @author james
 */
import ij.ImagePlus
import org.scalatest._
class NucleiSuite extends FunSuite with Matchers{
  val test_image_1_path = "/home/james/workspace/Zisa/test/test_image_1.tif"
  val test_image_1 = ImageIO.openImageFile(test_image_1_path)
  test("LoadImage"){
    test_image_1 shouldBe a [ij.ImagePlus]
  }
  
  test("processNuclei"){
    NucleiProcessing.processImageToNuclei(test_image_1)._1.length should equal (4)
  }
}