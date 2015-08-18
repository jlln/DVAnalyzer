




package Zisa

import ij.ImagePlus
import ij.IJ
import ij.plugin.ChannelSplitter
import ij.plugin.frame.RoiManager
import ij.measure.ResultsTable
import ij.plugin.filter.ParticleAnalyzer
import ij.WindowManager
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import ij.plugin.filter.Analyzer
import scala.runtime.ScalaRunTime._
import com.github.tototoshi.csv._
import java.io._
import scala.collection.JavaConversions._
import ij.gui.WaitForUserDialog
import ij.process.ByteProcessor
import scala.util.Random.nextInt
import scala.math.sqrt
import scala.math.pow


class Result(area:Double,labels:List[String],values:List[Option[Double]]){
  def getLabels = labels
  def getResultValues = values
  def getArea = area
  def concatenateResults(that:Result) = {
    val new_values:List[Option[Double]] = this.values ++that.getResultValues
    val new_labels:List[String] = this.labels ++that.getLabels
    new Result(area,new_labels,new_values)
  }
  def printResult{
    val interleaved = labels.zip(values)
    println(interleaved)
  }
}

class StringResult(data_result:Result,nucleus:Nucleus,image:String){
  val x_centre = nucleus.getXCentre.toString
  val y_centre = nucleus.getYCentre.toString
  val labels:List[String] = List("Filename","x_centre","y_centre")++data_result.getLabels
  val values:List[String] = List(image,x_centre,y_centre)++data_result.getResultValues.map{x=> x match{
    case Some(y) =>y.toString
    case None => "NA"
    }}
  def getLabels = labels
  def getValues = values.toSeq
}


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
class Nucleus(slices:Array[NucleusSlice]){
  def getSlices = slices.toList
  def getImageSlices = slices.toList.map{ s=> s.getSlice}
  def last = slices.last
  def append(s:NucleusSlice):Nucleus = new Nucleus(slices:+s)
  def getMaximumCrossSectionRoi = slices.zip(slices.map(s=>s.getArea)).maxBy(_._2)._1.getRoi
  def getTotalArea = (slices.map(s=>s.getArea)).sum
  def getMeanArea = ISA.mean(slices.map(s=>s.getArea))
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






object ISA {
  


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





  def iFind(pixels:List[Float],steps:List[(Double,Double)],step_size:Float):Double = {
    val mean_pixels = mean(pixels)
    val mean_minus_min = mean_pixels - pixels.min
    val threshold = 0.00003/((4*mean_pixels)*(0.25*mean_minus_min)*(standardDeviation(pixels)+pixels.max-pixels.min))

    steps.length match{
      //take the first two steps
      case x if x < 2 => iFind(pixels,takeStep(pixels,steps,step_size),step_size)
      case _ =>{
        val second_derivatives = steps.map{x=>x._2}
        val second_derivatives_variance = variance(second_derivatives)
        second_derivatives_variance match {
          case x if x < threshold => iFind(pixels,takeStep(pixels,steps,step_size),step_size) 
          case x if x > threshold => {
            steps.last._1
          }
        }
      }
    }  
  }

  def findInflection(image:Array[Array[Array[Float]]]):Double = {
    println("Finding Threshold")
    val pixels = image.flatten.flatten.toList.sorted
    val std_dev = standardDeviation(pixels)
    val u = mean(pixels)
    val max_divergence = pixels.max - u
    val min_divergence = u - pixels.min 
    val starting_step = pixels.max/100
    iFind(pixels,List(),starting_step)
    
  }


  def nucleiFocusser(nucleus:Nucleus,edge_mask:ij.ImagePlus):Nucleus = {
    var variance_results = new ResultsTable()
    val measurements = ij.measure.Measurements.MEAN+ij.measure.Measurements.AREA
    val analyzer= new Analyzer(edge_mask,measurements,variance_results)
    for (s<-nucleus.getSlices){
      edge_mask.setSlice(s.getSlice)
      edge_mask.setRoi(s.getRoi)
      analyzer.measure()
    }
    val variance_values1 = variance_results.getColumn(variance_results.getColumnIndex("Mean"))
    val variance_values=variance_values1.map(x => x*x)
    val area_values = variance_results.getColumn(variance_results.getColumnIndex("Area"))
    val mean_area = mean(area_values)
    val variance_threshold = 0.6*variance_values.max        
    val retained_slices = for ((s,i)<-nucleus.getSlices.zipWithIndex 
      if (variance_values(i)>variance_threshold & area_values(i)>mean_area*0.5)) yield s
    new Nucleus(retained_slices.toArray)
  }




  def threeChannelNucleusFocusser(edge_masks:List[ij.ImagePlus],nucleus:Nucleus):Nucleus={
    val retained_by_channel:List[Nucleus] = edge_masks.map{em=> nucleiFocusser(nucleus,em)}
    val retained_slices_by_channel = (retained_by_channel.map{n=>n.getSlices}).toList
    new Nucleus(((retained_slices_by_channel(0).intersect(retained_slices_by_channel(1))).intersect(retained_slices_by_channel(2))).toArray)
  }

  def threeChannelNucleiFocusser(r:ij.ImagePlus,g:ij.ImagePlus,b:ij.ImagePlus,nuclei:Array[Nucleus]):List[Nucleus]={
    println("Finding Focussed Slices")
    val edge_masks:List[ij.ImagePlus] = for (i <- List(r,g,b)) yield i.duplicate()
    edge_masks.map{em => IJ.run(em, "Find Edges","stack")}
    val focussed_nuclei:List[Nucleus] = (nuclei.map{n=>threeChannelNucleusFocusser(edge_masks,n)}).toList
    focussed_nuclei.filter{n=> n.getSlices.length > 0}


  }

  def processImageToNuclei(image:ij.ImagePlus):(List[Nucleus],Array[ImagePlus])={
    val channels:Array[ImagePlus]=ChannelSplitter.split(image)
    val (red,green,blue) = (channels(0),channels(1),channels(2))
    val (nuclei,nuclei_mask) = maskNuclei(blue)
    val focussed_nuclei = threeChannelNucleiFocusser(red,green,blue,nuclei)
    (focussed_nuclei,channels)
  }

  def getListOfSubDirectories(directoryName: String): Array[String] = 
       (new File(directoryName)).listFiles.filter(_.isDirectory).map(_.getName)
  
  def getListOfFilesInSubDirectory(directoryName: String): Array[String] = 
       (new File(directoryName)).listFiles.map(_.getName).filter(_.contains(".tif"))
  
  def mean[T](items:Traversable[T])(implicit n:Numeric[T]) = 
    n.toDouble(items.sum) / items.size.toDouble
  
  
  def variance[T](items:Traversable[T])(implicit n:Numeric[T]) : Double = {
    val itemMean = mean(items)
    val count = items.size
    val sumOfSquares = items.foldLeft(0.0d)((total,item)=>{
      val itemDbl = n.toDouble(item)
      val square = math.pow(itemDbl - itemMean,2)
      total + square
      })
    sumOfSquares / count.toDouble
  }
  
  def standardDeviation[T](items:Traversable[T])(implicit n:Numeric[T]) : Double = 
    scala.math.sqrt(variance(items))
  
  def standardScores[T](items:Traversable[T])(implicit n:Numeric[T]) : Traversable[Double] = {
    val sample_mean = mean(items)
    val sample_standard_deviation = standardDeviation(items)
    items map (x => (n.toDouble(x)-sample_mean)/sample_standard_deviation)
  }
  
  def eucledian[T](x1:T,y1:T,x2:T,y2:T)(implicit n:Numeric[T]):Double = {
  
    val dy = n.toDouble(y1) - n.toDouble(y2)
    val dx = n.toDouble(x1) - n.toDouble(x2)
    sqrt(pow(dx,2)+pow(dy,2))
  }

  def skewness[T](items:Traversable[T])(implicit n:Numeric[T]):Double = {
    val m = mean(items)
    val s = standardDeviation(items)
    val items_double = items.map{x=>n.toDouble(x)}
    val third_moment = items_double.map{x=>scala.math.pow((x.toDouble-m),3)}
    val summed_normed_third_moment = (third_moment.sum)/items.size.toDouble
    summed_normed_third_moment/scala.math.pow(s,3)
  }

  def kurtosis[T](items:Traversable[T])(implicit n:Numeric[T]):Double = {
    val m = mean(items)
    val s = standardDeviation(items)
    val items_double = items.map{x=>n.toDouble(x)}
    val third_moment = items_double.map{x=>scala.math.pow((x.toDouble-m),4)}
    val summed_normed_third_moment = (third_moment.sum)/items.size.toDouble
    summed_normed_third_moment/scala.math.pow(s,4)
  }

  

  def nucleusSliceEucledian(n1:NucleusSlice,n2:NucleusSlice):Double = {
    eucledian(n1.getXCentre,n1.getYCentre,n2.getXCentre,n2.getYCentre)
  }


   def openImageFile(filepath:String):ij.ImagePlus = {
    println("Loading Image: " + filepath)
    IJ.openImage(filepath)
  }

  def maskNuclei(blue:ImagePlus):(Array[Nucleus],ImagePlus)={

    WindowManager.closeAllWindows()
    val nuclei_mask=blue.duplicate()
    nuclei_mask.getChannelProcessor().resetMinAndMax()
    nuclei_mask.show
    val calibration = blue.getCalibration()
    calibration.setUnit("micron")
    val outlier_radius = calibration.getRawX(4.8)
    val blur_sigma = calibration.getRawX(1)
    val cell_lower = math.pow(calibration.getRawX(5),2)*3.14156
    val cell_upper = math.pow(calibration.getRawX(50),2)*3.14156
    val outlier_search_string = "radius="+outlier_radius+" threshold=50 which=Dark stack"
    IJ.run(nuclei_mask,"Gaussian Blur...", "sigma="+blur_sigma+" stack")
    IJ.run(nuclei_mask,"Make Binary", "method=RenyiEntropy background=Default calculate ")
    IJ.run(nuclei_mask,"Fill Holes", "stack")
    IJ.run(nuclei_mask,"Remove Outliers...", outlier_search_string);
    IJ.run(nuclei_mask,"Watershed", "stack")
    var roim= new RoiManager()
    var results_table= new ResultsTable()
    val pa = new ParticleAnalyzer(ParticleAnalyzer.ADD_TO_MANAGER,
    ij.measure.Measurements.MEAN+ij.measure.Measurements.CENTROID+ij.measure.Measurements.AREA,
    results_table,
    cell_lower,cell_upper,
    0.3,1.0)
    val stack_size = nuclei_mask.getStackSize()
    for (i<-(0 until stack_size)){
      nuclei_mask.setSliceWithoutUpdate(i + 1)
      pa.analyze(nuclei_mask)
      }
    val rm=roim.getRoisAsArray()
    val slice_list= for (roi<-rm) yield roi.getName().split("-")(0).toInt
    val roi_and_slice = (rm.zip(slice_list)).sortBy(_._2)  

    
    val x_centres = results_table.getColumn(results_table.getColumnIndex("X"))
    val y_centres = results_table.getColumn(results_table.getColumnIndex("Y"))
    val areas = results_table.getColumn(results_table.getColumnIndex("Area"))
    val processed_roi = ((((slice_list zip x_centres) zip y_centres) zip rm) zip areas) map {
          case ((((s,x),y),r),sr) => new NucleusSlice(s,x,y,r,sr)
          }
    var nuclei:Array[Nucleus]=Array()
    for (r <- processed_roi){
      nuclei=mergeNuclei(nuclei,r)
      }
    
    (nuclei,nuclei_mask)
    }
  

  def mergeNuclei(nuclei:Array[Nucleus],ro_i:NucleusSlice):Array[Nucleus]={
    
//  If a slice is found to be from an existing nucleus, merge it into that nucleus, else create a new nucleus.
//    How to discern if a slice belongs to a given nucleus? 
//    First find nuclei that are within the radius,
//    then from this set of nuclei (if there are more than one) choose the closest.
    val slice = ro_i.getSlice
    val area = ro_i.getArea
    val radius = scala.math.sqrt(area)/3.14156
    val x = ro_i.getXCentre
    val y = ro_i.getYCentre
    val close_nuclei = nuclei.filter( n=> (n.last.getXCentre - x).abs < 2*radius &
        (n.last.getYCentre-y).abs < 2*radius & n.last.getSlice==slice-1) 

    val distances = (close_nuclei map {n => nucleusSliceEucledian(n.last,ro_i) }).zip(close_nuclei)
    if (distances.length > 0){
      val closest = distances.minBy(_._1)._2
      val updated_closest_nucleus = closest.append(ro_i)
      nuclei.updated(nuclei.indexOf(closest),updated_closest_nucleus)
    }
    else {
      nuclei :+ new Nucleus(Array(ro_i))
    }    
  }
  
  
  def thresholdObjects(nucleus:Nucleus,channel:ij.ImagePlus):ij.ImagePlus = {
    val outer_bounds = nucleus.getBoundingBox
    val image_processors = for (s<-nucleus.getSlices) yield {
      channel.setSlice(s.getSlice)
      channel.setRoi(outer_bounds)
      val processor = channel.getProcessor().crop()
      processor
    }
    val pixel_array:Array[Array[Array[Float]]] = image_processors.toArray.map{ip=>
      ip.resetMinAndMax()
      ip.convertToFloat()
      ip.getFloatArray()
    }
    val threshold = findInflection(pixel_array)
    val width = image_processors.head.getWidth
    val height = image_processors.head.getHeight
    val output_stack = new ij.ImageStack(width,height)
    for (p<-image_processors){
      val ft = p.convertToFloat()
      val bp = p.convertToByte(false)
      val fta:Array[Array[Int]] = ft.getFloatArray().map{x=>
        x.map{y=>
          if (y>threshold) 255
          else 0
        }
      }

      bp.setIntArray(fta)
      output_stack.addSlice(bp)
    }
    val thresholded_image = new ImagePlus("thresholded",output_stack)
    IJ.run(thresholded_image,"Make Binary", "method=Default background=Default stack ")
    WindowManager.closeAllWindows()
    thresholded_image
  }



  def mergeResults(results:List[Result]):Result = {
    val area_multiplied_results:List[List[(Double,Double)]] = {
      (0 until results.head.getLabels.length).toList.map{
        l=>{
          results.map{r=>{
          val entry = r.getResultValues(l)
          entry match{
            case Some(x) => (x*r.getArea,r.getArea)
            case None => (0.0,0.0)
            }
            }}
          }
        }
      }
      val total_areas:List[Double] = area_multiplied_results.map{
        l=>l.map{x=>x._2}.sum
      }
      val area_multiplied_results_values:List[List[Double]]={
        area_multiplied_results.map{
        a=> a.map{
          r=>r._1}
        }
      }
      val corrected_values:List[Double] = area_multiplied_results_values.zip(total_areas).map{
        case (r,t)=>r.map{
          v=>v/t
          }.sum
        }
      val optioned_corrected_values = corrected_values.map{
      case x if x !=0.000 => Some(x)
      case x if x == 0.000 => None
      }
      val total_area = results.map{r=>r.getArea}.sum
      new Result(total_area,results.head.getLabels,optioned_corrected_values)
    }
  


  def analyzeNucleus(nucleus:Nucleus,images:Array[ImagePlus])(analyticalFunction:(NucleusSlice,Array[ImagePlus],Double,Double)
    =>Result):Result = {
    val calibration = images(0).getCalibration()
    calibration.setUnit("micron")
    val object_lower = math.pow(calibration.getRawX(1),2)*3.14156
    val object_upper = math.pow(calibration.getRawX(500),2)*3.14156
    val slices = nucleus.getSlices
    val areas = slices.map{s=> s.getArea}
    val raw_results = slices.map{s=>
      analyticalFunction(s,images,object_lower,object_upper)
    }
    mergeResults(raw_results)
  }

  def measureIntensities(nucleus_slice:NucleusSlice,channels:Array[ImagePlus],
    object_lower:Double,object_upper:Double):Result={
    val result_values:List[Option[Double]] = channels.toList.flatMap{ oc=>
        val pixels = nucleus_slice.getPixels(oc).flatten

        List(mean(pixels),standardDeviation(pixels),skewness(pixels),kurtosis(pixels))
        }.map{r=>Some(r)}
    val labels:List[String] = (1 until channels.length+1).toList.flatMap{
      c=> List(s"Channel$c"+"_MeanIntensity",s"Channel$c"+"_StandardDeviationIntensity",
        s"Channel$c"+"_SkewnessIntensity",s"Channel$c"+"_KurtosisIntensity")
    }
    new Result(nucleus_slice.getArea,labels,result_values)
  }


  def pearsonsPixelCorrelation(items_a:Array[Float],items_b:Array[Float],threshold_type:Int): Double = {
    // The threshold_type value can be 1 or 0, depending on if zero-x or zero-zero pixels are to be discarded. 
    // ie use 0 for standard correlation measurement, 
    // and use 1 for pure spatial intensity correlation in overlapping regions.
    val a_threshold = 10
    val b_threshold = 10
    val items_a_nz = items_a.map (x=>(if (x > a_threshold) 1 else 0))
    val items_b_nz = items_b.map (x=>(if (x > b_threshold) 1 else 0))
    val intersections = items_a_nz.zip(items_b_nz) map {case (a,b)=>a+b}
    val intersection_indices = intersections.zipWithIndex.filter(x => x._1 > threshold_type) map {case (z,i) =>i}
    val nz_items_a = intersection_indices.map (i=>items_a(i))
    val nz_items_b = intersection_indices.map (i=>items_b(i))
    val N = nz_items_a.size
    val z_scores_a = standardScores(nz_items_a)
    val z_scores_b = standardScores(nz_items_b)
    val r = (z_scores_a.toList.zip(z_scores_b.toList) map {case (za,zb) => za*zb}).sum/(N)
    r match {
      case x if x.isNaN => 0
      case x => x
    }
  }

  def pearsonCorrelation(nucleus_slice:NucleusSlice,channels:Array[ImagePlus],
    object_lower:Double,object_upper:Double):Result = {
    val channel_pairs = channels.combinations(2).toList
    val channel_pair_labels = (1 until channels.length+1).map(x=>x.toString).combinations(2).toList.map{
      case Vector(a,b)=> s"Channel$a"+"Pearson"+s"Channel$b"}
    val result_values = channel_pairs.map{
      case Array(a,b) =>{
        val channel_a:Array[Float] = nucleus_slice.getPixels(a).flatten
        val channel_b:Array[Float] = nucleus_slice.getPixels(b).flatten
        val pp = pearsonsPixelCorrelation(channel_a,channel_b,0)
        pp
      }
    }.map{r=>Some(r)}
    new Result(nucleus_slice.getArea,channel_pair_labels,result_values)
  }


  def nearestNeighbours(object_centroids:List[(Float,Float)]):List[Double] = {
    object_centroids.length match{
      case 0 => List(0)
      case 1 => List(0)
      case _ => object_centroids.map{
        c => {
          val other_points = object_centroids.filter{x=> x!= c}
          val distances = other_points.map{
            case (x,y) => {eucledian(c._1,c._2,x,y)}
          }
          distances.min
        }
      }
    }
  }


  def objectAnalysis(nucleus_slice:NucleusSlice,object_channels:Array[ImagePlus],
    object_lower:Double,object_upper:Double):Result = {
    val result_values:List[Option[Double]] = object_channels.flatMap{ oc=>
        oc.setSlice(nucleus_slice.getSlice)
        var roim= new RoiManager()
        var results_table= new ResultsTable()
        val pa = new ParticleAnalyzer(ParticleAnalyzer.ADD_TO_MANAGER,
          ij.measure.Measurements.MEAN+ij.measure.Measurements.CENTROID+ij.measure.Measurements.AREA,
          results_table,
          object_lower,object_upper,
          0,1.0)
        pa.analyze(oc)
        val areas_index = results_table.getColumnIndex("Area")
        if (areas_index == -1){
          List(Some(0.0),None,None,None,None,None,None,None,None,None,None,None,None)
        }
        else{
          val areas:Array[Float] = results_table.getColumn(areas_index)
          val x_centres:Array[Float] = results_table.getColumn(results_table.getColumnIndex("X"))
          val y_centres:Array[Float] = results_table.getColumn(results_table.getColumnIndex("Y"))
          val centroids = x_centres.zip(y_centres)
          val nearest_neighbours = nearestNeighbours(centroids.toList)
          val slice_centre_x = nucleus_slice.getXCentre
          val slice_centre_y = nucleus_slice.getYCentre
          val slice_area = nucleus_slice.getArea
          val radiality:Seq[Double] = centroids.map{
            case (x,y) => (eucledian(x,y,slice_centre_x,slice_centre_y))/slice_area
          }
          List(areas.length,mean(areas),standardDeviation(areas),skewness(areas),kurtosis(areas),
            mean(nearest_neighbours),standardDeviation(nearest_neighbours),
            skewness(nearest_neighbours),kurtosis(nearest_neighbours),
            mean(radiality),standardDeviation(radiality),
            skewness(radiality),kurtosis(radiality))
        }.map{r=>Some(r)}
        
      }.toList

    val channel_prefixes:List[String] = (1 until object_channels.length+1).toList.map{
      c=> s"Channel$c"+"_"
    }
       val label_suffixes:List[String] = List("ObjectCount","ObjectSizeMean","ObjectSizeStandardDeviation",
      "ObjectSizeSkewness","ObjectSizeKurtosis","ObjectNearestNeighbourDistanceMean",
      "ObjectNearestNeighbourDistanceStandardDeviation","ObjectNearestNeighbourDistanceSkewness",
      "ObjectNearestNeighbourDistanceKurtosis","ObjectRadialPositionMean"
      ,"ObjectRadialPositionStandardDeviation","ObjectRadialPositionSkewness","ObjectRadialPositionKurtosis")
    val labels:List[String] = for (c<-channel_prefixes;s<-label_suffixes) yield {
      c + s
    } 
    new Result(nucleus_slice.getArea,labels,result_values)

    
  }

  
  
  def main(args: Array[String]){
    val top_directory = new ij.io.DirectoryChooser("Choose Directory").getDirectory().toString
    val output_file = top_directory+"FullZisaAnalysis.csv"
    val csv_writer = CSVWriter.open(output_file,append=false)
    val headings = Seq("Filename", "x_centre", "y_centre", "Channel1_ObjectCount", "Channel1_ObjectSizeMean", "Channel1_ObjectSizeStandardDeviation", "Channel1_ObjectSizeSkewness", "Channel1_ObjectSizeKurtosis", 
      "Channel1_ObjectNearestNeighbourDistanceMean", "Channel1_ObjectNearestNeighbourDistanceStandardDeviation", "Channel1_ObjectNearestNeighbourDistanceSkewness", "Channel1_ObjectNearestNeighbourDistanceKurtosis", "Channel1_ObjectRadialPositionMean", 
      "Channel1_ObjectRadialPositionStandardDeviation", "Channel1_ObjectRadialPositionSkewness", "Channel1_ObjectRadialPositionKurtosis", "Channel2_ObjectCount", "Channel2_ObjectSizeMean", 
      "Channel2_ObjectSizeStandardDeviation", "Channel2_ObjectSizeSkewness", "Channel2_ObjectSizeKurtosis", "Channel2_ObjectNearestNeighbourDistanceMean", "Channel2_ObjectNearestNeighbourDistanceStandardDeviation", "Channel2_ObjectNearestNeighbourDistanceSkewness", 
      "Channel2_ObjectNearestNeighbourDistanceKurtosis", "Channel2_ObjectRadialPositionMean", "Channel2_ObjectRadialPositionStandardDeviation",
     "Channel2_ObjectRadialPositionSkewness", "Channel2_ObjectRadialPositionKurtosis", "Channel3_ObjectCount", "Channel3_ObjectSizeMean", "Channel3_ObjectSizeStandardDeviation", 
     "Channel3_ObjectSizeSkewness", "Channel3_ObjectSizeKurtosis", "Channel3_ObjectNearestNeighbourDistanceMean", "Channel3_ObjectNearestNeighbourDistanceStandardDeviation", "Channel3_ObjectNearestNeighbourDistanceSkewness", "Channel3_ObjectNearestNeighbourDistanceKurtosis", "Channel3_ObjectRadialPositionMean", 
     "Channel3_ObjectRadialPositionStandardDeviation", "Channel3_ObjectRadialPositionSkewness", "Channel3_ObjectRadialPositionKurtosis", "Channel1_MeanIntensity", "Channel1_StandardDeviationIntensity", "Channel1_SkewnessIntensity", 
     "Channel1_KurtosisIntensity", "Channel2_MeanIntensity", "Channel2_StandardDeviationIntensity", "Channel2_SkewnessIntensity", "Channel2_KurtosisIntensity", "Channel3_MeanIntensity", "Channel3_StandardDeviationIntensity", "Channel3_SkewnessIntensity", 
     "Channel3_KurtosisIntensity", "Channel1PearsonChannel2", "Channel1PearsonChannel3", "Channel2PearsonChannel3")
    csv_writer.writeRow(headings)
    for (subdirectory_name <- getListOfSubDirectories(top_directory)){
      for (file<-getListOfFilesInSubDirectory(top_directory+subdirectory_name)){
        val image =openImageFile(top_directory+subdirectory_name+"/"+file)
        val (nuclei,channels) = processImageToNuclei(image)
        for (n<-nuclei){
          val thresholded_channels = channels.map{c=>thresholdObjects(n,c)}
          val subnuclear_object_results = analyzeNucleus(n,thresholded_channels)(objectAnalysis)
          val intensity_results = analyzeNucleus(n,channels)(measureIntensities)
          val correlation_results = analyzeNucleus(n,channels)(pearsonCorrelation)
          val overall_result = subnuclear_object_results.concatenateResults(intensity_results.concatenateResults(correlation_results))
          val string_result = new StringResult(overall_result,n,file)
          println(string_result.getValues)
          csv_writer.writeRow(string_result.getValues)
          // subnuclear_object_results.printResult
          // intensity_results.printResult
          // correlation_results.printResult
        //   val image_processors = for (s<-n.getSlices) yield {
        //     val topo_image = channels(0)    
        //     topo_image.setSlice(s.getSlice)
        //     topo_image.setRoi(n.getBoundingBox)
        //     val processor = topo_image.getProcessor().crop()
        //     processor
        //   }
        //   val pixel_array:Array[Array[Array[Float]]] = image_processors.toArray.map{ip=>
        //     ip.convertToFloat()
        //     ip.getFloatArray()
        //   }
        //   // thresholdCountdown(pixel_array.flatten.flatten.toList)
        //   val threshold = findInflection(pixel_array)
        //   for (p<-image_processors){
        //     val ti = new ImagePlus("",p)
        //     ti.show()
        //     val ft = p.convertToFloat()
        //     val fta = ft.getFloatArray().map{x=>
        //       x.map{y=>
        //         if (y>threshold) y
        //         else 0
        //       }
        //     }

        //     ft.setFloatArray(fta)
        //     val ta = new ImagePlus("",ft)
        //     ta.show()
        //     IJ.run("Tile")
        //   }
        
        // Thread.sleep(2500)
        // WindowManager.closeAllWindows()


      }
    }
  }
  
  
  sys.exit(0)
}
}
