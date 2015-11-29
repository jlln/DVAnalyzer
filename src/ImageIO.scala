

/**
 * @author james
 */
import ij.ImagePlus
import ij.IJ
import ij.WindowManager
import java.io._
object ImageIO {
  def openImageFile(filepath:String):ij.ImagePlus = {
    println("Loading Image: " + filepath)
    IJ.openImage(filepath)
   }
  def makeImage(image:Array[Array[Int]]):ij.ImagePlus = {
    val image_height:Int = image.length
    val image_width:Int  = image.head.length
    val processor = new ij.process.ByteProcessor(image_height,image_width)
    processor.setIntArray(image)
    new ij.ImagePlus("thresholded_image",processor)
  }
  
  
  def drawPixels(image:ij.ImagePlus){
    image.show()
    ij.IJ.run("Tile")
    Thread.sleep(500)
    WindowManager.closeAllWindows()
  }
  
  
}

