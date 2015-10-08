

/**
 * @author james
 */
import ij.ImagePlus
import ij.IJ
import ij.WindowManager
object ObjectThresholding {
  
  def thresholdCount(pixels:List[Double],threshold:Double):Double={
    pixels.filter(x=>x>threshold).length.toDouble/pixels.length
  }

  def numericalSecondDerivative(pixels:List[Double],threshold:Double,h:Double):Double = {
    
    (thresholdCount(pixels,threshold+h)-2*thresholdCount(pixels,threshold)+thresholdCount(pixels,threshold-h))/(h*h)
  }
  
  
  
  def twoMeans[T](pixels:Traversable[T],group_one:Traversable[T],group_two:Traversable[T])(implicit n:Numeric[T]):Double = {
    val mean_one = Stats.mean(group_one)
    val mean_two = Stats.mean(group_two)
    val mid_point:Double = Stats.mean(List(mean_one,mean_two))
    val new_group_one = pixels.filter(p=> n.toDouble(p) < mid_point)
    val new_group_two = pixels.filter(p => n.toDouble(p) > mid_point)
    
   if (group_one == new_group_one && group_two == new_group_two) mid_point
   else{
     twoMeans(pixels,new_group_one,new_group_two)
   }
  }
  
  def threeMeans[T](pixels:Traversable[T],group_one:Traversable[T],group_two:Traversable[T],group_three:Traversable[T])(implicit n:Numeric[T]):Double = {
    val mean_one = Stats.mean(group_one)
    val mean_two = Stats.mean(group_two)
    val mean_three = Stats.mean(group_three)
    val mid_point_a:Double = Stats.mean(List(mean_one,mean_two))
    val mid_point_b:Double = Stats.mean(List(mean_two,mean_three))
    val new_group_one = pixels.filter(p=> n.toDouble(p) < mid_point_a)
    val new_group_two = pixels.filter(p => n.toDouble(p) > mid_point_a && n.toDouble(p) < mid_point_b)
    val new_group_three = pixels.filter(p => n.toDouble(p) > mid_point_b)
    
   if (group_one == new_group_one && group_two == new_group_two && group_three == new_group_three) mid_point_b
   else{
     threeMeans(pixels,new_group_one,new_group_two,new_group_three)
   }
  }
  
  def fourMeans[T](pixels:Traversable[T],group_one:Traversable[T],group_two:Traversable[T],group_three:Traversable[T],group_four:Traversable[T])(implicit n:Numeric[T]):Double = {
    val mean_one = Stats.mean(group_one)
    val mean_two = Stats.mean(group_two)
    val mean_three = Stats.mean(group_three)
    val mean_four = Stats.mean(group_four)
    val mid_point_a:Double = Stats.mean(List(mean_one,mean_two))
    val mid_point_b:Double = Stats.mean(List(mean_two,mean_three))
    val mid_point_c:Double = Stats.mean(List(mean_three,mean_four))
    val new_group_one = pixels.filter(p=> n.toDouble(p) < mid_point_a)
    val new_group_two = pixels.filter(p => n.toDouble(p) > mid_point_a && n.toDouble(p) < mid_point_b)
    val new_group_three = pixels.filter(p => n.toDouble(p) > mid_point_b && n.toDouble(p) < mid_point_c)
    val new_group_four = pixels.filter(p=>n.toDouble(p) > mid_point_c)
    
   if (group_one == new_group_one && group_two == new_group_two && group_three == new_group_three && group_four == new_group_four) mid_point_c
   else{
     fourMeans(pixels,new_group_one,new_group_two,new_group_three,new_group_four)
   }
  }
  
  
  
  
  def stableCountThreshold(increment:Double,pixels:List[Double],history:List[(Double,Double)],alpha:Double):Double = {
    val new_threshold = if (history.length == 0){ //take first step
      pixels.max - 2*increment
    }
    else {
      history.last._1 - increment
    }
    if (new_threshold >= pixels.max) stableCountThreshold(increment,pixels,List(),alpha+0.01)
    val current_second_derivative = numericalSecondDerivative(pixels,new_threshold,increment)
    val n_t = thresholdCount(pixels,new_threshold)
    val cutoff = 0.001
    val new_history = history:+(new_threshold,current_second_derivative)
    val prior_five = new_history.takeRight(20)
    val prior_derivatives = prior_five.map{x=>scala.math.abs(x._2)}
    if (prior_derivatives.max < cutoff) prior_five.head._1
    else{
      stableCountThreshold(increment,pixels,new_history,alpha)
    }
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
    val norm_pixels = Stats.standardScores(pixels).toList
    val increment = (pixels.max - pixels.min)/500
    val threshold = fourMeans(norm_pixels,List(norm_pixels.min),List(Stats.mean(norm_pixels)/3),List(Stats.mean(norm_pixels)/1.5),List(norm_pixels.max)) * Stats.standardDeviation(pixels)+Stats.mean(pixels)
    val width = image_processors.head.getWidth
    val height = image_processors.head.getHeight
    val output_stack = new ij.ImageStack(width,height)
    val output_array:Array[Array[Array[Int]]] = image_processors.map{p=>
      val original = new ij.ImagePlus("original",p)
      original.show()
      val ft = p.convertToFloat()
      val bp = p.convertToByte(false)
      val fta:Array[Array[Int]] = ft.getFloatArray().map{x=>
        x.map{y=>
          if (y>threshold) 0
          else 255
        }
      } 
      val thresholded = ImageIO.makeImage(fta)
      thresholded.show()


      fta
    }
    IJ.run("Tile")
    Thread.sleep(5000)
    WindowManager.closeAllWindows()
    (output_array,threshold)
     }
       

    
  
  
  }

