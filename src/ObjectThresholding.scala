

/**
 * @author james
 */
import ij.ImagePlus
import ij.IJ
import ij.WindowManager
object ObjectThresholding {

 
  def kMeansIter(items:List[Double],groups:List[List[Double]]):Double = {
    val group_means:List[Double] = groups.map(g=>Stats.mean(g))
    val group_boundaries = group_means.sliding(2).map{
      p=>Stats.mean(p)
    }.toList
    println(group_boundaries)
    val new_groups = (0 until group_boundaries.length).map{
      i=> i match{
        case 0 => items.filter(x=> x <= group_boundaries(i))
        case z => items.filter(x=> x <= group_boundaries(i) && x > group_boundaries(i-1))
      }
    }.toList
   println(new_groups)
   if (new_groups == groups) group_boundaries.last
   else kMeansIter(items,new_groups)
  }
  
  def kMeans[T](k:Int,items:Traversable[T])(implicit n: Numeric[T]):Double = {
    val ditems = items.toList.map(x=> n.toDouble(x))
    val interval = 2 * 1d/(k-1)
    val starting_groups:List[List[Double]] = (-1d until 1+interval by interval).toList.map(v => List(v))
    kMeansIter(ditems,starting_groups)
  }
  
  
  
  def fiveMeans[T](pixels:Traversable[T],group_one:Traversable[T],group_two:Traversable[T],group_three:Traversable[T],group_four:Traversable[T],group_five:Traversable[T])(implicit n:Numeric[T]):Double = {
    val mean_one = Stats.mean(group_one)
    val mean_two = Stats.mean(group_two)
    val mean_three = Stats.mean(group_three)
    val mean_four = Stats.mean(group_four)
    val mean_five = Stats.mean(group_five)
    val mid_point_a:Double = Stats.mean(List(mean_one,mean_two))
    val mid_point_b:Double = Stats.mean(List(mean_two,mean_three))
    val mid_point_c:Double = Stats.mean(List(mean_three,mean_four))
    val mid_point_d:Double = Stats.mean(List(mean_four,mean_five))
    val new_group_one = pixels.filter(p=> n.toDouble(p) < mid_point_a)
    val new_group_two = pixels.filter(p => n.toDouble(p) > mid_point_a && n.toDouble(p) < mid_point_b)
    val new_group_three = pixels.filter(p => n.toDouble(p) > mid_point_b && n.toDouble(p) < mid_point_c)
    val new_group_four = pixels.filter(p=>n.toDouble(p) > mid_point_c && n.toDouble(p) < mid_point_d)
    val new_group_five = pixels.filter(p=> n.toDouble(p) > mid_point_d)
    
   if (group_one == new_group_one && group_two == new_group_two && group_three == new_group_three && group_four == new_group_four && group_five == new_group_five) mid_point_d
   else{
     fiveMeans(pixels,new_group_one,new_group_two,new_group_three,new_group_four,new_group_five)
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

  def twoMeans[T](pixels:Traversable[T],group_one:Traversable[T],group_two:Traversable[T])(implicit n:Numeric[T]):Double = {
    val mean_one = Stats.mean(group_one)
    val mean_two = Stats.mean(group_two)


    val mid_point:Double = Stats.mean(List(mean_one,mean_two))


    val new_group_one = pixels.filter(p=> n.toDouble(p) < mid_point)
    val new_group_two = pixels.filter(p => n.toDouble(p) > mid_point)
    

    
   if (group_one == new_group_one && group_two == new_group_two ) mid_point
   else{
     twoMeans(pixels,new_group_one,new_group_two)
   }
  }

  
  
  
   def thresholdObjects(nucleus:Nucleus,image:ij.ImagePlus,mask:List[List[List[Int]]]):(Array[Array[Array[Int]]],Double) = {
    val outer_bounds = nucleus.getBoundingBox
    val image_processors = for (s<-nucleus.getSlices.toArray) yield {
      image.setSlice(s.getSlice)
      image.setRoi(outer_bounds)
      val processor = image.getProcessor().crop()
      processor.resetMinAndMax()
      processor
    }
    val pixel_array:Array[Array[Array[Float]]] = nucleus.getPixels(image,mask)
    
     
    val pixels:List[Float] = pixel_array.flatten.flatten.toList
    val norm_pixels = Stats.standardScores(pixels).toList
    val increment = (pixels.max - pixels.min)/500
//    val threshold = fourMeans(norm_pixels,List(norm_pixels.min),List(0.25),List(0.75),List(norm_pixels.max)) * Stats.standardDeviation(pixels)+Stats.mean(pixels)
    
    val threshold = fiveMeans(norm_pixels,List(-0.5d),List(0d),List(0.5d),List(1.5d),List(2d)) * Stats.standardDeviation(pixels)+Stats.mean(pixels)
    val width = pixel_array.head.head.length
    val height = pixel_array.head.length
    val output_stack = new ij.ImageStack(width,height)
    val output_array:Array[Array[Array[Int]]] = image_processors.zip(pixel_array).map{case(p,pa)=>
      val original = new ij.ImagePlus("original",p)
      original.show()
      val fta:Array[Array[Int]] = pa.flatten.map{
        p => if ( p > threshold) 255 else 0
      }.grouped(pa.head.length).toArray
      val thresholded = ImageIO.makeImage(fta)
      thresholded.show()


      fta
    }
    IJ.run("Tile")
    Thread.sleep(200 * output_array.length)
    WindowManager.closeAllWindows()
    (output_array,threshold)
     }
       
     
   
  def thresholdCount(pixels:List[Float],threshold:Double):Double={
    pixels.filter(x=>x>threshold).length.toDouble/pixels.length
  }

  def numericalSecondDerivative(pixels:List[Float],threshold:Double):Double = {
    val range = pixels.max - pixels.min
    (thresholdCount(pixels,threshold+0.01*range*threshold)-2*thresholdCount(pixels,threshold)+thresholdCount(pixels,threshold-0.01*range*threshold))/(0.01*range*threshold*0.01*range*threshold)
  }
  

  
  def findSCTThreshold(pixels:List[Float],current_threshold:Double,derivative_threshold_factor:Double,step_size:Double):Double = {
    if (current_threshold == pixels.max) findSCTThreshold(pixels,pixels.filter(_> 0).min,derivative_threshold_factor * 1.1,step_size)
    else{
      val second_derivative = numericalSecondDerivative(pixels,current_threshold)
      val cutoff = thresholdCount(pixels,current_threshold) * derivative_threshold_factor/1000
      if (scala.math.abs(second_derivative) < cutoff) current_threshold
      else{
        findSCTThreshold(pixels,current_threshold+step_size,derivative_threshold_factor,step_size)
      }
    }
   }
    
  
  def SCTWrapper(pixels:Array[Double]):Double = {
    val da_pixels = pixels.map(x=>x.toFloat).toList
    val lowest_non_zero = pixels.filter(_ > 0).min
    val step_size = (da_pixels.max - da_pixels.min)/da_pixels.length
    findSCTThreshold(da_pixels,lowest_non_zero,1,step_size)
  }
  
  
 
  
  
  }

