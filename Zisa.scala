// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, 
// INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
// IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, 
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
// ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

// This app is based around the ImageJ Java library. 
// Schneider, C.A., Rasband, W.S., Eliceiri, K.W. "NIH Image to ImageJ: 25 years of image analysis". Nature Methods 9, 671-675, 2012.

// There are lots of functions that aren't currently used. Their definitions still exist but they are never called.

import ij._
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
import scala.swing._

object ISA {


  case class NucleiSlice(slice:Int,x_centre:Double,y_centre:Double,roi:ij.gui.Roi,area:Double)
  case class Nuclei(slices:ListBuffer[NucleiSlice])
  case class ProcessedRoi(slice:Int,x_centre:Double,y_centre:Double,roi:ij.gui.Roi,area:Double)


  def getListOfSubDirectories(directoryName: String): Array[String] = {
      return (new File(directoryName)).listFiles.filter(_.isDirectory).map(_.getName)
  }
  def getListOfFilesInSubDirectory(directoryName: String): Array[String] = {
      return (new File(directoryName)).listFiles.map(_.getName).filter(_.contains(".tif"))
  }
  def mean[T](item:Traversable[T])(implicit n:Numeric[T]) = {
    n.toDouble(item.sum) / item.size.toDouble
  }

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


  def standardDeviation[T](items:Traversable[T])(implicit n:Numeric[T]) : Double = {

    scala.math.sqrt(variance(items))

  }


  def standardScores[T](items:Traversable[T])(implicit n:Numeric[T]) : Traversable[Double] = {
    val sample_mean = mean(items)
    val sample_standard_deviation = standardDeviation(items)
    val z_scores = items map (x => (n.toDouble(x)-sample_mean)/sample_standard_deviation)
    return z_scores
  }
  


  def subtractBackground(image:ij.ImagePlus,radius:Int):ImagePlus={
      for (i<-(1 until image.getStackSize)){
        image.setSlice(i)
        val str = "rolling="+radius.toString+" , slice"
        IJ.run(image,"Subtract Background...",str)
        }

      return image

    }

    def mergeNuclei(nuclei:ListBuffer[Nuclei],ro_i:ProcessedRoi):ListBuffer[Nuclei]={
    val slice = ro_i.slice
    val area = ro_i.area
    val radius = (scala.math.sqrt(area)/3.14156)
    val area_upper_limit = area*1.3
    val area_lower_limit = area*0.7
    val x = ro_i.x_centre
    val y = ro_i.y_centre
    for (n<-nuclei)
    {
      val last_slice = n.slices.last
      if (last_slice.x_centre > (x-radius) & last_slice.x_centre < (x+radius) 
        & last_slice.y_centre>(y-radius) & last_slice.y_centre < (y+radius) )
      {
        n.slices.append(NucleiSlice(ro_i.slice,ro_i.x_centre,ro_i.y_centre,ro_i.roi,ro_i.area))
        return nuclei
      }
      
    }
    val new_nucleus = Nuclei(ListBuffer(NucleiSlice(ro_i.slice,ro_i.x_centre,ro_i.y_centre,ro_i.roi,ro_i.area)))
    var new_nuclei:ListBuffer[Nuclei]=nuclei
    new_nuclei.append(new_nucleus)

    return new_nuclei  
      }

    // Identifies focussed slices within the cell/nucleus.
    def nucleiFocusser(nuclei:Nuclei,edge_mask:ij.ImagePlus):Nuclei = {
        var variance_results = new ResultsTable()
        val measurements = ij.measure.Measurements.MEAN+ij.measure.Measurements.AREA
        val analyzer= new Analyzer(edge_mask,measurements,variance_results)
        for (s<-nuclei.slices){
          edge_mask.setSlice(s.slice)
          edge_mask.setRoi(s.roi)
          analyzer.measure()
        }
        val variance_values1 = variance_results.getColumn(variance_results.getColumnIndex("Mean"))
        val variance_values=variance_values1.map(x => x*x)
        val area_values = variance_results.getColumn(variance_results.getColumnIndex("Area"))
        
        val mean_area = mean(area_values)
        val variance_threshold = 0.8*variance_values.max        
        
        val retained_slices = for ((s,i)<-nuclei.slices.zipWithIndex if (variance_values(i)>variance_threshold & area_values(i)>mean_area*0.7)) yield s
        
        return new Nuclei(retained_slices)
      }


    // Identifies the bounding box encompassing the cell throughout the z stack.
    def getNucleiBoundaries(nuclei:Nuclei):ij.gui.Roi={
      val start_x = for (s<-nuclei.slices) yield s.roi.getBounds().x
      val x=start_x.min
      val start_y = for(s<-nuclei.slices) yield s.roi.getBounds().y
      val y= start_y.max
      val widths = for (s<-nuclei.slices) yield s.roi.getBounds().width
      val w = widths.max
      val heights = for (s<-nuclei.slices) yield s.roi.getBounds().height
      val h = heights.max
      val bounds = new ij.gui.Roi(x,y,w,h)
      return bounds
      }





    // Thresholded colocalization
    def applyThreshold(pixels:Seq[Int],threshold:Int):(Array[Int])={

      val thresholded_pixels = for (p<-pixels)
                    yield{
                      if (p>threshold)
                      1
                      else
                      0
                    }
      return thresholded_pixels.toArray
    }
    // Overlaps
    // Calculate nucleus mean values
    def overlapWrapper(channel_a:Seq[Array[Int]],channel_b:Seq[Array[Int]]):(Double,Double)={
      val overlaps = channel_a.zip(channel_b) map {case (a,b) => overlapCoefficient(a,b)}
      val (o1,o2)= overlaps.unzip
      val mo1 = mean(o1)
      val mo2 = mean(o2)
      return (mo1,mo2)


    }

    def overlapCoefficient(pixels_a:Array[Int],pixels_b:Array[Int]):(Double,Double)={
      val intersection_array = pixels_a.zip(pixels_b) map {case(a,b)=>a+b}
      val intersections_count = (intersection_array.filter(x => x==2)).length
      val pixel_a_count = (pixels_a.filter(x => x==1)).length
      val pixel_b_count =(pixels_b.filter(x => x==1)).length
      val pixel_a_overlapping_fraction = intersections_count.toDouble/pixel_a_count
      val pixel_b_overlapping_fraction = intersections_count.toDouble/pixel_b_count
      return(pixel_a_overlapping_fraction,pixel_b_overlapping_fraction)
    }




    // Costes implementation
    // Regression A=bX+C
    def orthogonalRegression(pixels_a:Seq[Int],pixels_b:Seq[Int]):(Double,Double)={
      val line_a = regression(pixels_a,pixels_b)
      val line_b = regression(pixels_b,pixels_a)
      val m_a = line_a._1
      val c_a = line_a._2
      val m_b = 1/line_b._1
      val c_b = line_b._2/line_b._1
      val m = (m_a+m_b)/2
      val c = (c_a+c_b)/2
      return (m,c)

    }

    def regression(pixels_a:Seq[Int],pixels_b:Seq[Int]):(Double,Double)={
      val m_A = mean(pixels_a)
      val m_B = mean(pixels_b)
      val s_A = standardDeviation(pixels_a)
      val s_B = standardDeviation(pixels_b)
      val r = pearsonsCorrelationCoefficient(pixels_a,pixels_b)
      val m = r*(s_A/s_B)
      val c = m_A - m*m_B
      return (m,c)

    }

    // For ranking the pixels by intensity
    def rankSort(pixels:Seq[Int]):Seq[Int]={
      val sorted_pixels = pixels.sorted
      val unique_sorted_pixels = pixels.sorted.distinct
      val pixel_ranks = pixels.map(p=>unique_sorted_pixels.indexOf(p))
      return pixel_ranks
    }
    
    // All the nuclei in the image
    def costesFullImageWrapper(channel_a:List[List[Array[Int]]],channel_b:List[List[Array[Int]]]):(List[List[Array[Int]]],List[List[Array[Int]]])={
      println("Finding threshold using Coste's method...")
      val zipped_channels = channel_a.zip(channel_b)
      val zipped_processed_channels = zipped_channels map {case (a,b)=>costesNuclearWrapper(a,b)}
      val (a_thresholded,b_thresholded) = zipped_processed_channels.unzip
      return (a_thresholded,b_thresholded)

    }

    
    // Single Nucleus
    def costesNuclearWrapper(channel_a:List[Array[Int]],channel_b:List[Array[Int]]):(List[Array[Int]],List[Array[Int]])={
      val zipped_channels = channel_a.zip(channel_b)
      val zipped_processed_channels = zipped_channels map {case (a,b)=>costesWrapper(a,b)}
      val (a_thresholded,b_thresholded) = zipped_processed_channels.unzip
      return (a_thresholded,b_thresholded)


    }

    
    // Single slice of a nucleus
    def costesWrapper(pixels_a:Seq[Int],pixels_b:Seq[Int]):(Array[Int],Array[Int])={
      val rankings_a = rankSort(pixels_a)
      val rankings_b = rankSort(pixels_b)
      val lm = orthogonalRegression(pixels_b,pixels_a)
      val lm_gradient = lm._1
      val lm_intercept = lm._2
      val threshold = doCostes(pixels_a,pixels_b,rankings_a,0,lm_gradient,lm_intercept)
      val threshold_a = rankings_a(threshold)
      val threshold_b =rankings_b(threshold)
      val pixels_a_thresholded = applyThreshold(pixels_a,threshold_a)
      val pixels_b_thresholded = applyThreshold(pixels_b,threshold_b)
      return (pixels_a_thresholded,pixels_b_thresholded)

      

    }
  // Method used in the costes process
    def pearsonsCalculator(indices:Seq[Int],pixels_a:Seq[Int],pixels_b:Seq[Int]):Double={
      val included_a = indices.map(i=>pixels_a(i))
      val included_b = indices.map(i=>pixels_b(i))
      val pcc = pearsonsCorrelationCoefficient(included_a,included_b)
      return pcc
    }
    // The recursive costes function
    def doCostes(pixels_a:Seq[Int],pixels_b:Seq[Int],rankings_a:Seq[Int],cutoff_rank:Int,lm_gradient:Double,lm_intercept:Double):Int={
      val ranked_cutoff = rankings_a.max - cutoff_rank
      val threshold_index = rankings_a.indexOf(ranked_cutoff)
      
      if (ranked_cutoff == 0){
        return cutoff_rank
      }
      val threshold_a = pixels_a(threshold_index)
      val threshold_b = lm_gradient*threshold_a + lm_intercept
      
      val retained_a = for ((p,i)<-pixels_a.zipWithIndex if (p > threshold_a)) yield i
      val retained_b = for ((p,i)<-pixels_b.zipWithIndex if (p > threshold_b)) yield i
      val pcc = retained_a.length - retained_b.length match {
        case x if x < 0 => pearsonsCalculator(retained_b,pixels_a,pixels_b)
        case _ => pearsonsCalculator(retained_a,pixels_a,pixels_b)

      }
      
      if (pcc*pcc>0.0225){
        doCostes(pixels_a,pixels_b,rankings_a,cutoff_rank+1,lm_gradient,lm_intercept)
      }else{
        return cutoff_rank
      }

      
    }


    // MeanThresholdedManders
    def meanManders(pixels_a:Array[Int],pixels_b:Array[Int]):Double={
      val mean_a = mean(pixels_a)
      val mean_b = mean(pixels_b)
      
      val thresholded_pixels_a = for (p<-pixels_a)
                    yield{
                      if (p>mean_a)
                      1
                      else
                      0
                    }
      val thresholded_pixels_b = for (p<-pixels_b)
                    yield{
                      if (p>mean_b)
                      1
                      else
                      0
                    }

    val matches = thresholded_pixels_a.zip(thresholded_pixels_b) map {case (a,b)=>a+b}
    val match_count = matches.filter(x=> x==2).length
    val score_a_on_b = match_count.toDouble/thresholded_pixels_a.sum
    return score_a_on_b
    }

    def meanMandersSliceWrapper(channel_a:List[Array[Int]],channel_b:List[Array[Int]]):Double={
      val per_slice_results = channel_a.zip(channel_b) map {case (a,b) => meanManders(a,b)}
      val nuclei_mean = mean(per_slice_results)
      return nuclei_mean
    }
    
    def meanMandersNuclearWrapper(channel_a:List[List[Array[Int]]],channel_b:List[List[Array[Int]]]):List[Double]={
      val zipped_nuclei = channel_a.zip(channel_b)
      val results = zipped_nuclei map {case (a,b)=>meanMandersSliceWrapper(a,b)}
      return results
    }
    
    


    // Gets the pixel values for the passed channel within the passed nuclei. Returns list of Int arrays.
    def getNucleiPixels(image:ij.ImagePlus,nuclei:Nuclei,mask:List[Array[Int]] = null):List[Array[Int]]={
      
      val boundaries = getNucleiBoundaries(nuclei)
      val pixel_array_by_slice = for (s<-nuclei.slices) yield {
        image.setSlice(s.slice)
        
        image.setRoi(boundaries)
        val cropped_image = image.getProcessor.crop()
        
        val pixels_array1:Array[Short] = cropped_image.getPixels().asInstanceOf[Array[Short]]
        val pixels_array = pixels_array1 map {x=>x.toInt}

        pixels_array
        }
      val masked_pixel_array = mask match {
        case null => pixel_array_by_slice
        case _ => pixel_array_by_slice zip (mask) map {case (p,m) => p zip(m) map {case (pp,mm) => (if (mm >0 ) pp else 0)}}
      }
      return pixel_array_by_slice.toList
      }






    def maskNuclei(blue:ImagePlus):(List[Nuclei],ImagePlus)={
    val nuclei_mask=blue.duplicate()
    nuclei_mask.getChannelProcessor().resetMinAndMax()
    val calibration = blue.getCalibration()
    calibration.setUnit("micron")
    val outlier_radius = calibration.getRawX(4.8)
    val blur_sigma = calibration.getRawX(1)
    val outlier_search_string = "radius="+outlier_radius+" threshold=50 which=Dark stack"
    IJ.run(nuclei_mask,"Gaussian Blur...", "sigma="+blur_sigma+" stack")
    IJ.run(nuclei_mask,"Make Binary", "method=RenyiEntropy background=Default calculate")
    IJ.run(nuclei_mask,"Fill Holes", "stack")
    IJ.run(nuclei_mask,"Remove Outliers...", outlier_search_string);
    IJ.run(nuclei_mask,"Dilate","stack")
    IJ.run(nuclei_mask,"Watershed", "stack")

    var roim= new RoiManager()
    var results= new ResultsTable()
    val pa = new ParticleAnalyzer(ParticleAnalyzer.ADD_TO_MANAGER,
    ij.measure.Measurements.MEAN+ij.measure.Measurements.CENTROID+ij.measure.Measurements.AREA,
    results,
    1000,5000000,
    0,1.0)
    for (i<-(0 until nuclei_mask.getStackSize())){
      nuclei_mask.setSliceWithoutUpdate(i + 1)
      pa.analyze(nuclei_mask)
      nuclei_mask.show()
      
      }

    val rm=roim.getRoisAsArray()
    val slice_list= for (roi<-rm) yield roi.getName().split("-")(0).toInt
    val roi_and_slice = (rm.zip(slice_list)).sortBy(_._2)  

    val x_centres = results.getColumn(results.getColumnIndex("X")).toList
    val y_centres = results.getColumn(results.getColumnIndex("Y")).toList
    val areas = results.getColumn(results.getColumnIndex("Area")).toList
    val processed_roi = ((((slice_list zip x_centres) zip y_centres) zip rm) zip areas) map {
          case ((((s,x),y),r),sr) => ProcessedRoi(s,x,y,r,sr)
          }
        var nuclei:ListBuffer[Nuclei]=ListBuffer()
    for (r <- processed_roi){
      nuclei=mergeNuclei(nuclei,r)
      }
    val edge_mask = blue.duplicate()
        IJ.run(edge_mask, "Find Edges","stack")
        
        val focussed_nuclei = (for (n<-nuclei) yield nucleiFocusser(n,edge_mask)).filter(x=> x.slices.length !=0)
        println("Nuclei identified")
        return (focussed_nuclei.toList,nuclei_mask)
    }


  def visualCheck(image:ij.ImagePlus,nuclei:List[Nuclei]){
      
        for (n<-nuclei){
          val boundaries = getNucleiBoundaries(n)
      for (s<-n.slices){
        image.setSlice(s.slice)

        image.setRoi(boundaries)
        val cropped_image = image.getProcessor.crop()
        val preview_image = new ij.ImagePlus("i",cropped_image)
        preview_image.getChannelProcessor().resetMinAndMax()
        preview_image.show()
        Thread.sleep(100)
        WindowManager.closeAllWindows()
        }
          }
      }

      // For now, due to troubles with trying to interface the imageJ API with scala, only the middle slice of each nuclei is examined for object stats.
    def makeObjectMask(image:ij.ImagePlus,nuclei:Nuclei):ImagePlus = { 
      val boundaries = getNucleiBoundaries(nuclei)
      val middle_slice = math.floor(image.getStackSize()/2)
      image.setSlice(middle_slice.toInt)
      image.setRoi(boundaries)
      val cropped_image = image.getProcessor.crop()
    val object_mask=subtractBackground(new ImagePlus("",cropped_image),10)
    IJ.run(object_mask,"Make Binary", "method=Default background=Default calculate")
    return object_mask
    }


    def makeObjectMasks(image:ij.ImagePlus,nuclei:List[Nuclei]):List[ImagePlus]={ 
      val masks = nuclei.map (n=> makeObjectMask(image,n))
      WindowManager.closeAllWindows()
      return masks
      }

    def pearsonsCorrelationCoefficient[T](items_a:Traversable[T],items_b:Traversable[T])(implicit n:Numeric[T]): Double = {

    val N = items_a.size
    val z_scores_a = standardScores(items_a)
    val z_scores_b = standardScores(items_b)
    val r_value = ( (z_scores_a.toList.zip(z_scores_b.toList) map {case (za,zb) => za*zb}).sum/(N) )
    return r_value
    }

// Process all the slices in the cell
  def pearsonsIntermediateWrapper(a:List[Array[Int]],b:List[Array[Int]]):Double={
    val zipped_slices = a.zip(b)
    val r_values = zipped_slices map {case (a,b) => pearsonsCorrelationCoefficient(a,b)}
    val m = mean(r_values)
    m match{
      case x if x.isNaN => return 0
      case y => return y
    }
    }


  def pearsonsNonZeroIntermediateWrapper(a:List[Array[Int]],b:List[Array[Int]]):Double={
    val zipped_slices = a.zip(b)
    val r_values = zipped_slices map {case (a,b) => pearsonsNonZeroPixelCorrelation(a,b)}
    val m = mean(r_values)
    m match{
      case x if x.isNaN => return 0
      case y => return y
    }
    }

  def pearsonsNonZeroPixelCorrelation(items_a:Array[Int],items_b:Array[Int]): Double = {
    val a_threshold = 1
    val b_threshold = 1
    val items_a_nz = items_a.map (x=>(if (x > a_threshold) 1 else 0))
    val items_b_nz = items_b.map (x=>(if (x > b_threshold) 1 else 0))
    val intersections = items_a_nz.zip(items_b_nz) map {case (a,b)=>a+b}
    val intersection_indices = intersections.zipWithIndex.filter(x => x._1 > 1) map {case (z,i) =>i}
    val nz_items_a = intersection_indices.map (i=>items_a(i))
    val nz_items_b = intersection_indices.map (i=>items_b(i))
    val N = nz_items_a.size
    val z_scores_a = standardScores(nz_items_a)
    val z_scores_b = standardScores(nz_items_b)
    val r_value = ( (z_scores_a.toList.zip(z_scores_b.toList) map {case (za,zb) => za*zb}).sum/(N) )
    return r_value
  }

    // Process all the cells in the image
  def pearsonsWrapper(channel_a:List[List[Array[Int]]],channel_b:List[List[Array[Int]]]):List[Double] = {
    val zipped_nuclei = channel_a.zip(channel_b)
    val r_values = zipped_nuclei map {case(a:List[Array[Int]],b:List[Array[Int]]) =>pearsonsIntermediateWrapper(a,b) }
    return r_values
    }






  def random_image_translator(channel_a:List[Array[Int]],channel_b:List[Array[Int]],width:Int,height:Int,mask:List[Array[Int]]):Double={
    
    val slices_by_rows = for(s<-channel_a) yield s.toList.grouped(width).toList
    val mask_slices_by_rows = for (s<-mask) yield s.toList.grouped(width).toList
    

    val dx=nextInt(width)
    val dy=nextInt(height)
    val x_translated_slices_by_rows = for (s<-slices_by_rows) yield s.map(r=>r.slice(dx,r.length)++r.slice(0,dx))
    val x_translated_mask_slices_by_rows = for (s<-mask_slices_by_rows) yield s.map(r=>r.slice(dx,r.length)
      ++r.slice(0,dx))
    val flattened_xy_translated_slices = for (s<-x_translated_slices_by_rows) yield (s.slice(dy,s.length)
      ++s.slice(0,dy)).flatten.toArray
    val flattened_xy_translated_mask_slices:List[Array[Int]] = for (s<-x_translated_mask_slices_by_rows) yield (s.slice(dy,s.length)++s.slice(0,dy)).flatten.toArray
    val zipped_mask_slices:List[Array[(Int,Int)]] = for ((ts,s)<-flattened_xy_translated_mask_slices.zip(mask)) yield ts.zip(s)
    val intersections:List[Array[Int]] = for (s<-zipped_mask_slices) yield s.map {case (a,b) => a+b}
    
    val intersection_indices = for (s<-intersections) yield s.zipWithIndex filter {case (v,i) => v== 510} map {case (value,index) => index }
    val retained_a = for ((s_a,s_m)<-channel_a.zip(intersection_indices)) yield s_m.map (i=>s_a(i))
    val retained_b = for ((s_b,s_m)<-channel_b.zip(intersection_indices)) yield (s_m).map (i=>s_b(i))
    val pcc = pearsonsIntermediateWrapper(retained_a,retained_b)
    return pcc
    
  }


  // Randomly xy displaces the pixels of a single nucleus stack, with the pixels wrapping to the opposite end of the image.
  def randomizerWrapper(channel_a:List[Array[Int]],channel_b:List[Array[Int]],width:Int,masking_channel:List[Array[Int]]):Double={
    println("Testing significance...")
    val length = channel_a(0).length
    val height = length/width
    val original_pcc = pearsonsIntermediateWrapper(channel_a,channel_b)
    val pccs = (1 until 500).par map {_ => random_image_translator(channel_a,channel_b,width,height,masking_channel)}
    val below_threshold = pccs.filter (x=> x.abs < original_pcc.abs)
    val p_value = below_threshold.length.toDouble/500
    println(p_value)
    return p_value
  }

    // Significance Testing. Wrapper accepts a list of nuclei, each comprising a list of slices, each of which is an array of shorts.
    // Also accepts a list of image widths. Returns a list, which consists of a list of the PCC values obtained for each nucleus over 500 iterations of 
    // the constrained displacement algorithm.
  def randomizationTestingWrapper(channel_a:List[List[Array[Int]]],
    channel_b:List[List[Array[Int]]],
    widths:List[Int],
    masking_channel:List[List[Array[Int]]]):List[Double]={
    val inputs = ((channel_a,channel_b,widths).zipped,masking_channel).zipped
    val p_values=inputs map {case((a,b,w),m)=>randomizerWrapper(a,b,w,m)}
    return p_values.toList
  }

  
  def measureNucleusIntensity(channel:ImagePlus,nucleus:Nuclei):Double = {
    val measurements=ij.measure.Measurements.MEAN
        var results= new ResultsTable()
        val analyzer = new Analyzer(channel, measurements, results)
        for (s<-nucleus.slices){
          channel.setSlice(s.slice)
          channel.setRoi(s.roi)
          analyzer.measure()
        }
        val intensities = results.getColumn(results.getColumnIndex("Mean"))
        return mean(intensities)

  }


  def measureNucleiIntensities(channel:ImagePlus,nuclei:List[Nuclei]):List[Double]={
    val intensities = nuclei map {x=> measureNucleusIntensity(channel,x)}
    return intensities

  }

  //For the analysis of subnuclear objects.
  def object_mask_analyzer(object_mask:ImagePlus):(Int,Float,Float)={
    var roim= new RoiManager()
    var results= new ResultsTable()
    val pa = new ParticleAnalyzer(ParticleAnalyzer.ADD_TO_MANAGER,
    ij.measure.Measurements.MEAN+ij.measure.Measurements.CENTROID+ij.measure.Measurements.AREA,
    results,
    15,5000000,
    0,1.0)
    for (i<-(0 until object_mask.getStackSize())){
      object_mask.setSliceWithoutUpdate(i + 1)
      pa.analyze(object_mask)
      }
    val areas = results.getColumn(results.getColumnIndex("Area")).toList
    val object_count = areas.length
    val mean_object_area = mean(areas).toFloat
    val object_area_sd = standardDeviation(areas).toFloat
    return (object_count,mean_object_area,object_area_sd)
  }

  //For the determination of mean nucleus slice area
  def getNucleusMeanArea(nucleus:Nuclei):Float={
    val areas = for (s<-nucleus.slices) yield s.area
    return mean(areas).toFloat
  }
  
  def processImageFile(filepath:String,csv_writer:CSVWriter,explanatory_value:String){
    val opener = new ij.io.Opener()
    val image:ImagePlus = opener.openImage(filepath)
    println("Finding nuclei and focusing...")
    
    val channels:Array[ImagePlus]=ChannelSplitter.split(image)
    val blue = channels(2)
    val green = subtractBackground(channels(1),15)
    val red = subtractBackground(channels(0),15)
    val (focussed_nuclei,nuclei_mask) = maskNuclei(blue)
    val mask_float = new ij.ImagePlus("mask", nuclei_mask.getProcessor().convertToShort(false))
    val mask_pixels = focussed_nuclei map (x=>getNucleiPixels(mask_float,x))
    visualCheck(blue,focussed_nuclei)
    println("Analysing...")
    val green_masks = makeObjectMasks(green,focussed_nuclei)
    val red_masks = makeObjectMasks(red,focussed_nuclei)
    
    val blue_pixels = focussed_nuclei.zipWithIndex map {case (x,i) => getNucleiPixels(blue,x,mask_pixels(i))}
    val green_pixels = focussed_nuclei.zipWithIndex map {case (x,i) => getNucleiPixels(green,x,mask_pixels(i))}
    val red_pixels = focussed_nuclei.zipWithIndex map {case (x,i) => getNucleiPixels(red,x,mask_pixels(i))}

    val image_widths = focussed_nuclei map (x=>getNucleiBoundaries(x).getBounds().width)
    val coloc_p_values = randomizationTestingWrapper(green_pixels,red_pixels,image_widths,mask_pixels)

    val nucleus_areas = focussed_nuclei.map(n=>getNucleusMeanArea(n))
  
    val green_objects = green_masks.map(x => object_mask_analyzer(x))
    val (green_object_count,green_object_mean_area,green_object_sd_area)=green_objects.unzip3
    val red_objects = red_masks.map(x => object_mask_analyzer(x))
    val (red_object_count,red_object_mean_area,red_object_sd_area)=red_objects.unzip3
    

    
    
    val mrog = meanMandersNuclearWrapper(red_pixels,green_pixels)
    val mgor = meanMandersNuclearWrapper(green_pixels,red_pixels)
    val rg_r_values = pearsonsWrapper(green_pixels,red_pixels) 
    val bg_r_values = pearsonsWrapper(blue_pixels,green_pixels) 
    val rb_r_values = pearsonsWrapper(blue_pixels,red_pixels) 
    
    val green_intensities = measureNucleiIntensities(green,focussed_nuclei) 
    val red_intensities = measureNucleiIntensities(red,focussed_nuclei)

    val rows = for (i <-nucleus_areas.indices) yield (List(explanatory_value,nucleus_areas(i)
      ,green_intensities(i),red_intensities(i),rg_r_values(i),bg_r_values(i),rb_r_values(i)
      ,red_object_count(i),red_object_mean_area(i),green_object_count(i),
      green_object_mean_area(i),mgor(i),mrog(i),coloc_p_values(i)))
    for (r<-rows){
      println(r)
      csv_writer.writeRow(r)
    WindowManager.closeAllWindows()
    }
    
    
    

    }

  

  def main(args: Array[String]){
    // val top_directory = "/Users/work/Documents/h2ax_time_series_2/"

    val top_directory = new ij.io.DirectoryChooser("Choose Directory").getDirectory().toString
    val output_file = top_directory+"ZisaAnalysis.csv"
    val csv_writer = CSVWriter.open(output_file,append=false)
    csv_writer.writeRow(Seq("ExplanatoryValue","Nucleus Area","GreenIntensity","RedIntensity",
      "GreenRedPearson","GreenBluePearson","RedBluePearson","RedObjectCount","RedObjectMeanArea",
      "GreenObjectCount","GreenObjectMeanArea","MeanThresholdOverlapGOR","MeanThresholdOverlapROG",
      "ColocPValue"))
    
    
    for (subdirectory_name <- getListOfSubDirectories(top_directory)){
      
      for (file<-getListOfFilesInSubDirectory(top_directory+subdirectory_name)){
        println(top_directory+subdirectory_name+"/"+file)
        processImageFile(top_directory+subdirectory_name+"/"+file,csv_writer,subdirectory_name)
        }
      }


    }


}
