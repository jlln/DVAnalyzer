




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
  def getPixels(image:ij.ImagePlus):Array[Int] = {
    val boundaries = getBoundingBox
    image.setRoi(boundaries)
    image.setSlice(getSlice)
    val cropped_image = image.getProcessor.crop()
    val pixel_array:Array[Float] = cropped_image.getFloatArray().flatten
    pixel_array.map(x=>x.toInt)
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
  def getOverlayRoi:List[ij.gui.Roi] = {
    val roi = getMaximumCrossSectionRoi
    for (s<-getSlices) yield {
      val sroi = roi
      sroi.setPosition(s.getSlice)
      sroi
    }
  }
  def getPixels(image:ij.ImagePlus):List[Array[Int]] = {
    for (s<-getSlices) yield s.getPixels(image)
  }

      
}



object ISA {
  
  def getListOfSubDirectories(directoryName: String): Array[String] = 
       (new File(directoryName)).listFiles.filter(_.isDirectory).map(_.getName)
  
  def getListOfFilesInSubDirectory(directoryName: String): Array[String] = 
       (new File(directoryName)).listFiles.map(_.getName).filter(_.contains(".tif"))
  
  def mean[T](item:Traversable[T])(implicit n:Numeric[T]) = 
    n.toDouble(item.sum) / item.size.toDouble
  
  
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
  
  def nucleusSliceEucledian(n1:NucleusSlice,n2:NucleusSlice):Double = {
    eucledian(n1.getXCentre,n1.getYCentre,n2.getXCentre,n2.getYCentre)
  }
    
  
  
  def subtractBackground(image:ij.ImagePlus,radius:Int,slices:List[Int]):ImagePlus={
    // println("Subtracting Background")
    for (i<-slices){
      image.setSlice(i)
      val str = "rolling="+radius.toString+" , slice"
      IJ.run(image,"Subtract Background...",str)
      }
    image
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
  
  // Accepts a list of nuclei or slice areas, and an array of results arrays for the corresponding slices or nuclei.
  // Scales each value within each result array by the area of the slice/nuclei
  def scaleResultsByTotalNuclearArea(areas:Seq[Double],results:List[List[Double]]):List[Double] = {
    val results_zipped_area = results.zip(areas.toList)
    val total_area = areas.sum
    val corrected_values = results_zipped_area map { case (r,a) => r.map(x=>x*a/total_area)}
    (corrected_values.transpose map (_.sum))

  }


  def analyzeNuclei(input_image:ij.ImagePlus,nuclei:List[Nucleus]):List[List[Double]] = {

    // For each experimental condition - each nucleus has a series of numerical attributes, and a total area. When calculating the population average
    // the values for each nucleus should be scaled according to its area.
    // Within each nucleus, each slice has an area - the nuclear averages across slices should also be scaled by area.
    // Each type of measurement is a function that applies to a single slice of a nucleus ie a NucleusSlice object, returning a double
    // A curried wrapper function will produce area-weighted averages across the slices of each nuclei using a provided function, returning a double.
    // Each nucleus will be described by these various measurements, and by its total area.
    // Each type of measurement will return three values, since there are three channels (also giving three possible pairwise combinations of channels)
    // nuclei map {n => analyzeNucleus(image,n)(measureSliceIntensity)}

    println("Analyzing Nuclei")
    val slices:List[Int] = (nuclei map {n => n.getImageSlices}).flatten.distinct
    val image = subtractBackground(input_image,2,slices)


    (nuclei map {n=> List(n.getTotalArea) ++ analyzeNucleus(image,n,true)(measureSliceIntensity) ++ analyzeNucleus(image,n,true)(slicePearsons(0)) ++ analyzeNucleus(image,n,false)(analyzeSubnuclearObjects)}).toList
    
    
  }

  def analyzeNucleus(image:ij.ImagePlus,nucleus:Nucleus,all_slices:Boolean)(analyticalFunction:(NucleusSlice, ij.ImagePlus,ij.ImagePlus,ij.ImagePlus) 
    => List[Double]):List[Double] = {
    val nuclear_slices = nucleus.getSlices
    val middle_slice = (math.floor(nuclear_slices.length/2)).toInt
    val slices = all_slices match {
      case true => nuclear_slices
      case false => nuclear_slices.length match {                               //Just using the most central slices
        case x if x > 4 => nuclear_slices.slice(middle_slice -2,middle_slice+3)
        case x if x < 5 => nuclear_slices
      }
                            
        
    }
    val areas = slices.map(s=>s.getArea)
    val channels:Array[ImagePlus]=ChannelSplitter.split(image)
    val (red,green,blue) = (channels(0),channels(1),channels(2))
    val raw_values:List[List[Double]] = for (s <- slices) yield {
      analyticalFunction(s,red,green,blue)
    }
    scaleResultsByTotalNuclearArea(areas,raw_values)
    

  }



 

  // Curried analytical functions are here: Each function takes a NucleusSlice and three images (one per channel) to return an Array of Doubles.


  // Measure slice intensity.
  def measureSliceIntensity(slice:NucleusSlice,r:ij.ImagePlus,g:ij.ImagePlus,b:ij.ImagePlus):List[Double] = {
    // println("Measuring Pixel Intensities")
    r.setSlice(slice.getSlice)
    r.setRoi(slice.getRoi)
    val measurements=ij.measure.Measurements.MEAN
    val results = new ResultsTable()
    val r_analyzer = new Analyzer(r, measurements, results)
    val g_analyzer = new Analyzer(g,measurements,results)
    val b_analyzer = new Analyzer(b,measurements,results)
    r_analyzer.measure
    g_analyzer.measure
    b_analyzer.measure
    val result = results.getColumn(results.getColumnIndex("Mean"))
    List(result(0).toDouble,result(1).toDouble,result(2).toDouble)
  }

  // The threshold value can be 1 or 0, depending on if zero-x or zero-zero pixels are to be discarded. ie use 0 for standard correlation 
  // measurement, and use 1 for pure spatial intensity correlation in overlapping regions.
  def slicePearsons(threshold:Int)(slice:NucleusSlice,r:ij.ImagePlus,g:ij.ImagePlus,b:ij.ImagePlus):List[Double] = {
    // println("Calculting Pearson's Correlation Coefficient")
    val r_pixels = slice.getPixels(r)
    val g_pixels = slice.getPixels(g)
    val b_pixels = slice.getPixels(b)
    val rg = pearsonsPixelCorrelation(r_pixels,g_pixels,threshold)
    val rb = pearsonsPixelCorrelation(r_pixels,b_pixels,threshold)
    val gb = pearsonsPixelCorrelation(g_pixels,b_pixels,threshold)
    List(rg,rb,gb)
  }

  def pearsonsPixelCorrelation(items_a:Array[Int],items_b:Array[Int],threshold_type:Int): Double = {
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
  


  def analyzeObjects(slice:Int,boundaries:ij.gui.Roi,object_mask:ij.ImagePlus):List[Double] = {
    
    
    IJ.run("Tile")
    // println("Analyzing Objects")
    val calibration = object_mask.getCalibration()
    calibration.setUnit("micron")
    val object_lower = math.pow(calibration.getRawX(1),2)*3.14156
    val object_upper = math.pow(calibration.getRawX(80),2)*3.14156
    var roim= new RoiManager()
    var results= new ResultsTable()
    val pa = new ParticleAnalyzer(ParticleAnalyzer.ADD_TO_MANAGER,
      ij.measure.Measurements.MEAN+ij.measure.Measurements.CENTROID+ij.measure.Measurements.AREA,
      results,
      object_lower,object_upper,
      0,1.0)
    object_mask.show()
    IJ.run("Tile")
    pa.analyze(object_mask)
    results.getColumnIndex("Area") match {
      case -1 => List(0,0,0)
      case _ => {
        val areas = results.getColumn(results.getColumnIndex("Area"))
        val centroids = results.getColumn(results.getColumnIndex("X")).zip( results.getColumn(results.getColumnIndex("Y")) )
        println(centroids.toList)
        val object_count = areas.length.toDouble
        val mean_object_area = mean(areas).toDouble
        val object_area_sd = standardDeviation(areas).toDouble
        List(object_count,mean_object_area,object_area_sd)
      }
    }
  }

  

  def mandersOverlapCoefficient(channel_a:ij.ImagePlus,channel_b:ij.ImagePlus):List[Double] = {
    val pixels_a = channel_a.getProcessor.getFloatArray().flatten map{x=>x.toInt}
    val pixels_b = channel_b.getProcessor.getFloatArray().flatten map{x=>x.toInt}
    val sum = pixels_a.zip(pixels_b) map {case(a,b) => a+b}
    val n_pixels = pixels_a.length.toDouble
    val aob = sum.zip(pixels_a).filter(x=> x._1==2 && x._2==1).size.toDouble
    val boa = sum.zip(pixels_b).filter(x=> x._1==2 && x._2==1).size.toDouble
    List(aob/n_pixels, boa/n_pixels)
  }



  def prepareObjectChannel(channel:ij.ImagePlus,background_radius:Double,slice:Int,boundaries:ij.gui.Roi):ij.ImagePlus = {
    WindowManager.closeAllWindows
    channel.setSlice(slice)
    channel.setRoi(boundaries)
    val cropped_image = channel.getProcessor.crop()
    val c_i = subtractBackground(new ImagePlus("",cropped_image),4,List(1))
    val original_image_processor = cropped_image.duplicate()
    original_image_processor.resetMinAndMax()
    val original_image = new ImagePlus("original",original_image_processor)
    original_image.show()
    val c_ip = c_i.getProcessor
    c_ip.setAutoThreshold("Otsu dark") 
    c_ip.convertToByte(true)

    val object_mask = new ImagePlus("object_mask",c_ip)
    object_mask.show()
    IJ.run("Tile")
    // Thread.sleep(500)
    object_mask
  }


  def getObjectCount(object_mask:ij.ImagePlus):Double = {
    var roim= new RoiManager()
    var results= new ResultsTable()
    val pa = new ParticleAnalyzer(ParticleAnalyzer.NOTHING,
      ij.measure.Measurements.AREA,
      results,
      0,100000000,
      0,1.0)
    pa.analyze(object_mask)
    results.getColumnIndex("Area") match {
      case -1 =>0
      case _ => results.getColumn(results.getColumnIndex("Area")).length * results.getColumn(results.getColumnIndex("Area")).sum
    }
  }


  def newThresoldFunction(channel:ij.process.ImageProcessor):Double={
    val counts:Seq[Double] = for (t<- 1 until 256) yield {
      val new_channel = channel.duplicate()
      new_channel.setThreshold(t,255,3)
      val count = getObjectCount(new ImagePlus("",new_channel))
      count
    }
    val counts_with_thresholds = counts.zipWithIndex.sortBy(_._1)
    val best = counts_with_thresholds(254)
    val best_threshold = best._2.toDouble
    best_threshold
  }

  


  def analyzeSubnuclearObjects(slice:NucleusSlice,r:ij.ImagePlus,g:ij.ImagePlus,b:ij.ImagePlus):List[Double] = {
    // Performs object analsysis and Mander's overlap analsis
    // Returns r - object count , r - mean object area, r-object area sd, and then same for g and b.
    WindowManager.closeAllWindows
    val current_slice = slice.getSlice
    val boundaries = slice.getBoundingBox
    val calibration = r.getCalibration()
    calibration.setUnit("micron")
    val background_radius = calibration.getRawX(0.5)
    val r_objects = prepareObjectChannel(r,background_radius,current_slice,boundaries)
    val r_results = analyzeObjects(current_slice,boundaries,r_objects)
    val g_objects = prepareObjectChannel(g,background_radius,current_slice,boundaries)
    val g_results = analyzeObjects(current_slice,boundaries,g_objects)
    val b_objects = prepareObjectChannel(b,background_radius,current_slice,boundaries)
    val b_results = analyzeObjects(current_slice,boundaries,b_objects)
    List(r_results,g_results,b_results).flatten
    
  }



  def visualCheck(image:ij.ImagePlus,nuclei:List[Nucleus]){
    val channels:Array[ImagePlus]=ChannelSplitter.split(image)
    val (red,green,blue) = (channels(0),channels(1),channels(2))
    for (n<-nuclei){
      val boundaries = n.getBoundingBox
      for (s<-n.getSlices){
        image.setSlice(s.getSlice)
        image.setRoi(boundaries)
        val cropped_image = image.getProcessor.crop()
        val preview_image = new ij.ImagePlus("i",cropped_image)
        preview_image.getChannelProcessor().resetMinAndMax()
        preview_image.show()
        // Thread.sleep(100)
        WindowManager.closeAllWindows()
        }
      }
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
    val variance_threshold = 0.8*variance_values.max        
    val retained_slices = for ((s,i)<-nucleus.getSlices.zipWithIndex 
      if (variance_values(i)>variance_threshold & area_values(i)>mean_area*0.7)) yield s
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

  def processImageToNuclei(image:ij.ImagePlus):List[Nucleus]={
    val channels:Array[ImagePlus]=ChannelSplitter.split(image)
    val (red,green,blue) = (channels(0),channels(1),channels(2))
    val (nuclei,nuclei_mask) = maskNuclei(blue)
    val focussed_nuclei = threeChannelNucleiFocusser(red,green,blue,nuclei)
    focussed_nuclei
  }
  
  def openImageFile(filepath:String):ij.ImagePlus = {
    println("Loading Image: " + filepath)
    IJ.openImage(filepath)
  }
  
  def main(args: Array[String]){
    val top_directory = new ij.io.DirectoryChooser("Choose Directory").getDirectory().toString
    val full_output_file = top_directory+"FullZisaAnalysis.csv"
    val object_output_file = top_directory+"ObjectZisaAnalysis.csv"
    val full_csv_writer = CSVWriter.open(full_output_file,append=false)
    full_csv_writer.writeRow(Seq("ExperimentalCondition","totalarea","intensity_red","intensity_green","intensity_blue","pearsons_rg","pearsons_rb","pearsons_gb","object_count_red","mean_object_area_red","object_area_sd_red","object_count_green","mean_object_area_green","object_area_sd_green","object_count_blue","mean_object_area_blue","object_area_sd_blue"))
      
    for (subdirectory_name <- getListOfSubDirectories(top_directory)){
      for (file<-getListOfFilesInSubDirectory(top_directory+subdirectory_name)){
        val image =openImageFile(top_directory+subdirectory_name+"/"+file)
        val nuclei = processImageToNuclei(image)
        val results = analyzeNuclei(image,nuclei)
        val result_rows:List[List[String]] = results.map{r=> r.map{x=>x.toString}}
        val rows:List[Seq[String]] = result_rows.map {r=> subdirectory_name.toString +: r}
        for (r<-rows) {
          println(r)
          full_csv_writer.writeRow(r)
          WindowManager.closeAllWindows()
        }
      }
    }
  }
  
  
  
}
