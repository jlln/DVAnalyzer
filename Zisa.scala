// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, 
// INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
// IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, 
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
// ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

// This app is based around the ImageJ Java library. 
// Schneider, C.A., Rasband, W.S., Eliceiri, K.W. "NIH Image to ImageJ: 25 years of image analysis". Nature Methods 9, 671-675, 2012.

// jlln.github.com

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
		    & last_slice.y_centre>(y-radius) & last_slice.y_centre < (y+radius) 
		    & last_slice.area > area_lower_limit & last_slice.area < area_upper_limit)
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
    	val lm = regression(pixels_b,pixels_a)
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





    // Gets the pixel values for the passed channel within the passed nuclei. Returns list of short arrays.
    def getNucleiPixels(image:ij.ImagePlus,nuclei:Nuclei):List[Array[Int]]={
      var pixel_array_by_slice:ListBuffer[Array[Int]] = ListBuffer()
      val boundaries = getNucleiBoundaries(nuclei)
      for (s<-nuclei.slices){
        image.setSlice(s.slice)
        
        image.setRoi(boundaries)
        val cropped_image = image.getProcessor.crop()
        
        val pixels_array1:Array[Short] = cropped_image.getPixels().asInstanceOf[Array[Short]]
        val pixels_array = pixels_array1 map {x=>x.toInt}

        pixel_array_by_slice.append(pixels_array)
        }
      return pixel_array_by_slice.toList
    	}

    def maskNuclei(blue:ImagePlus):List[Nuclei]={
    	val nuclei_mask=blue.duplicate()
		nuclei_mask.getChannelProcessor().resetMinAndMax()
		val calibration = blue.getCalibration()
		calibration.setUnit("micron")
		val outlier_radius = calibration.getRawX(4.8)
		val outlier_search_string = "radius="+outlier_radius+" threshold=50 which=Dark stack"
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
		3000,5000000,
		0,1.0)
		for (i<-(0 until nuclei_mask.getStackSize())){
			nuclei_mask.setSliceWithoutUpdate(i + 1)
			pa.analyze(nuclei_mask)
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
      	return focussed_nuclei.toList
		}


	def visualCheck(image:ij.ImagePlus,nuclei:List[Nuclei]){
      
      	for (n<-nuclei){
      		val boundaries = getNucleiBoundaries(n)
			for (s<-n.slices){
				image.setSlice(s.slice)

				image.setRoi(boundaries)
				val cropped_image = image.getProcessor.crop()
				val preview_image = new ij.ImagePlus("i",cropped_image)
				// preview_image.getProcessor.resetMinAndMax()
				preview_image.show()
				Thread.sleep(100)
				WindowManager.closeAllWindows()
				}
      		}
    	}

      // For now, due to troubles with trying to interface the imageJ API with scala, only the first slice of each nuclei is examined for object stats.
    def makeObjectMask(image:ij.ImagePlus,nuclei:Nuclei):ImagePlus = { 
    	val boundaries = getNucleiBoundaries(nuclei)
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

	def pearsonsIntermediateWrapper(a:List[Array[Int]],b:List[Array[Int]]):Double={
		val zipped_slices = a.zip(b)
		val r_values = zipped_slices map {case (a,b) => pearsonsCorrelationCoefficient(a,b)}
		val m = mean(r_values)
		m match{
			case x if x.isNaN => return 0
			case y => return y
		}
		}

	def pearsonsWrapper(channel_a:List[List[Array[Int]]],channel_b:List[List[Array[Int]]]):List[Double] = {
		val zipped_nuclei = channel_a.zip(channel_b)
		val r_values = zipped_nuclei map {case(a:List[Array[Int]],b:List[Array[Int]]) =>pearsonsIntermediateWrapper(a,b) }
		return r_values
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
		
		val channels:Array[ImagePlus]=ChannelSplitter.split(image)
		val blue = channels(2)
		val green = subtractBackground(channels(1),15)
		val red = subtractBackground(channels(0),15)
		val focussed_nuclei = maskNuclei(blue)
		
		
		println("Analysing...")
		val green_masks = makeObjectMasks(green,focussed_nuclei)
		val red_masks = makeObjectMasks(red,focussed_nuclei)
		
		val blue_pixels = focussed_nuclei map (x=> getNucleiPixels(blue,x))
		val green_pixels = focussed_nuclei map (x=> getNucleiPixels(green,x))
		val red_pixels = focussed_nuclei map (x=> getNucleiPixels(red,x))
		val nucleus_areas = focussed_nuclei.map(n=>getNucleusMeanArea(n))
	
		val green_objects = green_masks.map(x => object_mask_analyzer(x))
    val (green_object_count,green_object_mean_area,green_object_sd_area)=green_objects.unzip3
    val red_objects = red_masks.map(x => object_mask_analyzer(x))
    val (red_object_count,red_object_mean_area,red_object_sd_area)=red_objects.unzip3
		

    val (green_thresholded, red_thresholded) = costesFullImageWrapper(green_pixels,red_pixels)

		

	
		val rg_thresholded = pearsonsWrapper(green_thresholded,red_thresholded)
		
		val overlap_data = green_thresholded.zip(red_thresholded) map {case (g,r) =>overlapWrapper(g,r)}
		val (gr_overlap,rg_overlap) = overlap_data.unzip

		val gr_overlapw = gr_overlap.map(a=>a.toString)
		val rg_overlapw = rg_overlap.map(a=>a.toString)
		
		
		val rg_r_values = pearsonsWrapper(green_pixels,red_pixels) 
		val bg_r_values = pearsonsWrapper(blue_pixels,green_pixels) 
		val rb_r_values = pearsonsWrapper(blue_pixels,red_pixels) 
		val green_intensities = measureNucleiIntensities(green,focussed_nuclei) 
		val red_intensities = measureNucleiIntensities(red,focussed_nuclei)

		val rows = for (i <-nucleus_areas.indices) yield (List(explanatory_value,nucleus_areas(i),green_intensities(i),red_intensities(i),rg_r_values(i),bg_r_values(i),rb_r_values(i),rg_thresholded(i),gr_overlapw(i),rg_overlapw(i),red_object_count(i),red_object_mean_area(i),green_object_count(i),green_object_mean_area(i)))
		for (r<-rows){
			println(r)
			csv_writer.writeRow(r)
		WindowManager.closeAllWindows()
		}
		
		
		

		}

	

	def main(args: Array[String]){
		val top_directory = new ij.io.DirectoryChooser("Choose Directory").getDirectory().toString
		val output_file = top_directory+"ZisaAnalysis.csv"
		val csv_writer = CSVWriter.open(output_file,append=false)
		csv_writer.writeRow(Seq("ExplanatoryValue","Nucleus Area","GreenIntensity","RedIntensity","GreenRedPearson","GreenBluePearson","RedBluePearson","PearsonRGThresholded","GreenOverlapRed","RedOverlapGreen","RedObjectCount","RedObjectMeanArea","GreenObjectCount","GreenObjectMeanArea"))
		
		// val top_directory = "/Users/work/Documents/h2ax_time_series_2/"
		for (subdirectory_name <- getListOfSubDirectories(top_directory)){
			
			for (file<-getListOfFilesInSubDirectory(top_directory+subdirectory_name)){
				println(top_directory+subdirectory_name+"/"+file)
				processImageFile(top_directory+subdirectory_name+"/"+file,csv_writer,subdirectory_name)
				}
			}


		}


}
