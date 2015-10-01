

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

  def numericalSecondDerivative(pixels:List[Float],threshold:Double):Double = {
    
    (thresholdCount(pixels,threshold+0.011*threshold)-2*thresholdCount(pixels,threshold)+thresholdCount(pixels,threshold-0.011*threshold))/(0.011*threshold*0.011*threshold)
  }
  

  
  def findThreshold(pixels:List[Float],current_threshold:Double,threshold_history:List[(Double,Double)],derivative_threshold:Double):Double = {
    val current_second_derivative = numericalSecondDerivative(pixels,current_threshold)
    val running_mean_second_derivative = Stats.mean(threshold_history.takeRight(3).map(x=>x._2))
//    println(current_threshold,current_second_derivative)
    val pixel_mean = Stats.mean(pixels)
    val last_five_derivatives = threshold_history.takeRight(10).map{_._2} :+ current_second_derivative
    if (threshold_history.length < 4) findThreshold(pixels:List[Float],current_threshold*1.02,threshold_history:+(current_threshold,current_second_derivative),derivative_threshold)
    else if (last_five_derivatives.map(x=>scala.math.abs(x)).max < derivative_threshold) threshold_history.takeRight(10).head._1
    else findThreshold(pixels:List[Float],current_threshold*1.01,threshold_history:+(current_threshold,current_second_derivative),derivative_threshold)
   }
  
  

  
   def thresholdObjects(nucleus:Nucleus,image:ij.ImagePlus):(Array[Array[Array[Int]]],Double) = {
    val outer_bounds = nucleus.getBoundingBox
    val image_processors = for (s<-nucleus.getSlices.toArray) yield {
      image.setSlice(s.getSlice)
      image.setRoi(outer_bounds)
      val processor = image.getProcessor().crop()
      processor
    }
    val pixel_array:Array[Array[Array[Float]]] = image_processors.map{ip=>
      ip.resetMinAndMax()
      ip.convertToFloat()
      ip.getFloatArray()
    }
     
    val pixels:List[Float] = pixel_array.flatten.flatten.toList
    val derivative_threshold = 0.0000003
    val threshold = findThreshold(pixels,pixels.min+100,List(),derivative_threshold)
    val width = image_processors.head.getWidth
    val height = image_processors.head.getHeight
    val output_stack = new ij.ImageStack(width,height)
    val output_array:Array[Array[Array[Int]]] = image_processors.map{p=>
      val ft = p.convertToFloat()
      val bp = p.convertToByte(false)
      val fta:Array[Array[Int]] = ft.getFloatArray().map{x=>
        x.map{y=>
          if (y>threshold) 0
          else 255
        }
      } 
      fta
    }
    WindowManager.closeAllWindows()
    (output_array,threshold)
    
     (output_array,threshold)
     }
       

    
  
  
  }

