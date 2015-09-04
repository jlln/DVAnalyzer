

/**
 * @author james
 */
class Nucleus(slices:Array[NucleusSlice]){
  def getSlices = slices.toList
  def getImageSlices = slices.toList.map{ s=> s.getSlice}
  def last = slices.last
  def append(s:NucleusSlice):Nucleus = new Nucleus(slices:+s)
  def getMaximumCrossSectionRoi = slices.zip(slices.map(s=>s.getArea)).maxBy(_._2)._1.getRoi
  def getTotalArea = (slices.map(s=>s.getArea)).sum
  def getMeanArea = Stats.mean(slices.map(s=>s.getArea))
  def getBoundingBox = {
    val start_x:Array[Int] = for (s<-slices) yield s.getRoi.getBounds().x
    val x=start_x.min
    val start_y:Array[Int] = for(s<-slices) yield s.getRoi.getBounds().y
    val y= start_y.max
    val widths:Array[Int] = for (s<-slices) yield s.getRoi.getBounds().width
    val w = widths.max
    val heights:Array[Int] = for (s<-slices) yield s.getRoi.getBounds().height
    val h = heights.max
    new ij.gui.Roi(x,y,w,h)
    
  }
  def getXCentre = slices.head.getXCentre
  def getYCentre = slices.head.getYCentre

  def getOverlayRoi:List[ij.gui.Roi] = {
    val roi = getMaximumCrossSectionRoi
    for (s<-getSlices) yield {
      val sroi = roi
      sroi.setPosition(s.getSlice)
      sroi
    }
  }
  
  def getPixels(image:ij.ImagePlus):Array[Array[Array[Float]]] = {
    val boundaries = getBoundingBox
    val processors:Array[ij.process.ImageProcessor] = getSlices.toArray.map{ s=>
      s.makeCroppedProcessor(image,boundaries)
    }
    processors.map{ p=>
      p.getFloatArray().flatten.grouped(boundaries.getBounds().width).toArray
    }
  }

      
}