

/**
 * @author james
 */
class NucleusSlice(slice:Int,x_centre:Double,y_centre:Double,roi:ij.gui.Roi,area:Double){
  def getSlice = slice
  def getXCentre = x_centre
  def getYCentre = y_centre
  def getCentroid = (x_centre,y_centre)
  def getRoi = roi
  def getArea = area
  def getBoundingBox = {
    val x = getRoi.getBounds().x
    val y = getRoi.getBounds().y
    val w = getRoi.getBounds().width
    val h = getRoi.getBounds().height
    new ij.gui.Roi(x,y,w,h)
  }
  def makeCroppedProcessor(image:ij.ImagePlus,boundaries:ij.gui.Roi):ij.process.ImageProcessor = {
    image.setRoi(boundaries)
    image.setSlice(getSlice)
    image.getProcessor.crop()
  }
  def getPixels(image:ij.ImagePlus):Array[Array[Float]] = {
    val cropped_image = makeCroppedProcessor(image,getBoundingBox)
    val pixel_array:Array[Float] = cropped_image.getFloatArray().flatten
    pixel_array.grouped(getRoi.getBounds().width).toArray
  }

}