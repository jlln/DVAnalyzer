

/**
 * @author james
 */
import ij.ImagePlus
import ij.IJ
import ij.WindowManager
object ObjectThresholding {
  
  def thresholdCount(pixels:List[Float],threshold:Double):Double={
    pixels.filter(x=>x>threshold).length.toDouble/pixels.length
  }

  def numericalSecondDerivative(pixels:List[Float],threshold:Double,h:Double):Double = {
    val interval = {
      if (h < 2){
        5
      }
      else{
        h
      }
    }
    (thresholdCount(pixels,threshold+interval)-2*thresholdCount(pixels,threshold)+thresholdCount(pixels,threshold-interval))/(interval*interval)
  }
  
  def takeStep(pixels:List[Float],steps:List[(Double,Double)],step_size:Float):List[(Double,Double)] = {
   val new_threshold = { 
      if (steps.isEmpty){
        pixels.max - step_size
      }
      else{
        steps.last._1 - step_size
      }
    }
    val new_derivative = numericalSecondDerivative(pixels,new_threshold,step_size)
    steps:+(new_threshold,new_derivative)
  }
  
  def iFind(threshold:Double,pixels:List[Float],steps:List[(Double,Double)],step_size:Float):Double = {
    steps.length match{
      //take the first two steps
      case x if x < 2 => iFind(threshold,pixels,takeStep(pixels,steps,step_size),step_size)
      case _ =>{
        if (steps.last._1 < 0) iFind(threshold,pixels,List(),step_size/2)
        val second_derivatives = steps.map{x=>x._2}
        val second_derivatives_running_mean = Stats.mean(second_derivatives)
        println(second_derivatives_running_mean,threshold)
        second_derivatives_running_mean match {
          case x if x < threshold => iFind(threshold,pixels,takeStep(pixels,steps,step_size),step_size) 
          case x if x > threshold => steps.last._1         
        }
      }
    }  
  }
  def findInflection(threshold:Double,image:Array[Array[Array[Float]]]):Double = {
    println("Finding Threshold")
    val pixels = image.flatten.flatten.toList.sorted
    val std_dev = Stats.standardDeviation(pixels)
    val u = Stats.mean(pixels)
    val max_divergence = pixels.max - u
    val min_divergence = u - pixels.min 
    val starting_step = pixels.max/200
    iFind(threshold,pixels,List(),starting_step)
  }
  
  
  
  def thresholdTestFunction(deriv_threshold:Double,image:Array[Array[Float]]):Array[Array[Int]] = {
    val threshold = findInflection(deriv_threshold,Array(image))
    val image_width = image.head.length
    val output_array = image.flatten.map{p=> if (p > threshold) 255 else 0}
    output_array.grouped(image_width).toArray
  }
  
  def thresholdObjects(nucleus:Nucleus,channel:ij.ImagePlus):(Array[Array[Array[Int]]],Double) = {
    val outer_bounds = nucleus.getBoundingBox
    val image_processors = for (s<-nucleus.getSlices.toArray) yield {
      channel.setSlice(s.getSlice)
      channel.setRoi(outer_bounds)
      val processor = channel.getProcessor().crop()
      processor
    }
    val pixel_array:Array[Array[Array[Float]]] = image_processors.map{ip=>
      ip.resetMinAndMax()
      ip.convertToFloat()
      ip.getFloatArray()
    }
    val pixels:Array[Float] = pixel_array.flatten.flatten
    val mean_pixels = Stats.mean(pixels)
    val deriv_threshold = 0.000000000020 * mean_pixels
    val threshold = findInflection(deriv_threshold,pixel_array)
//    println(threshold)
    val width = image_processors.head.getWidth
    val height = image_processors.head.getHeight
    val output_stack = new ij.ImageStack(width,height)
    val output_array:Array[Array[Array[Int]]] = image_processors.map{p=>
      val ft = p.convertToFloat()
      val bp = p.convertToByte(false)
      val fta:Array[Array[Int]] = ft.getFloatArray().map{x=>
        x.map{y=>
          if (y>threshold) 255
          else 0
        }
      } 
      fta
    }
    WindowManager.closeAllWindows()
    (output_array,threshold)
  }

}