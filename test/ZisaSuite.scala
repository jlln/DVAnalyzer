

/**
 * @author james
 */
import org.scalatest._
class ZisaSuite extends FunSuite {
  val test_image_1_path = "/home/james/workspace/Zisa/test/test_image_1.tif"
  val test_analysis = Zisa.processImage("test",test_image_1_path)
  println(test_analysis)
}