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

    // Gets the pixel values for the passed channel within the passed nuclei. Returns list of short arrays.
    def getNucleiPixels(image:ij.ImagePlus,nuclei:Nuclei):List[Array[Short]]={
      var pixel_array_by_slice:ListBuffer[Array[Short]] = ListBuffer()
      val boundaries = getNucleiBoundaries(nuclei)
      for (s<-nuclei.slices){
        image.setSlice(s.slice)
        
        image.setRoi(boundaries)
        val cropped_image = image.getProcessor.crop()
        
        val pixels_array:Array[Short] = cropped_image.getPixels().asInstanceOf[Array[Short]]
        pixel_array_by_slice.append(pixels_array)
        }
      return pixel_array_by_slice.toList
    	}

    def maskNuclei(blue:ImagePlus):List[Nuclei]={
    	val nuclei_mask=blue.duplicate()
		nuclei_mask.getChannelProcessor().resetMinAndMax()
		println("Identifying the nuclei...")
		IJ.run(nuclei_mask,"Variance...", "radius=2 stack")
		IJ.run(nuclei_mask,"Make Binary", "method=RenyiEntropy background=Default calculate")
		IJ.run(nuclei_mask,"Fill Holes", "stack")
		IJ.run(nuclei_mask,"Remove Outliers...", "radius=30 threshold=50 which=Dark stack");
		IJ.run(nuclei_mask,"Mean...", "radius=15 stack")
		IJ.run(nuclei_mask,"Make Binary", "method=Default background=Default calculate")
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
      	nuclei = for (n<-nuclei if n.slices.length >9) yield n
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

	def pearsonsIntermediateWrapper(a:List[Array[Short]],b:List[Array[Short]]):Double={
		val zipped_slices = a.zip(b)
		val r_values = zipped_slices map {case (a,b) => pearsonsCorrelationCoefficient(a,b)}
		return mean(r_values)
		}

	def pearsonsWrapper(channel_a:List[List[Array[Short]]],channel_b:List[List[Array[Short]]]):List[Double] = {
		val zipped_nuclei = channel_a.zip(channel_b)
		val r_values = zipped_nuclei map {case(a:List[Array[Short]],b:List[Array[Short]]) =>pearsonsIntermediateWrapper(a,b) }
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
		visualCheck(red,focussed_nuclei)
		
		println("Analysing...")
		val green_masks = makeObjectMasks(green,focussed_nuclei)
		val red_masks = makeObjectMasks(red,focussed_nuclei)
		val green_objects = green_masks.map(x => object_mask_analyzer(x))
		val (green_object_count,green_object_mean_area,green_object_sd_area)=green_objects.unzip3
		val red_objects = red_masks.map(x => object_mask_analyzer(x))
		val (red_object_count,red_object_mean_area,red_object_sd_area)=red_objects.unzip3
		val blue_pixels = focussed_nuclei map (x=> getNucleiPixels(blue,x))
		val green_pixels = focussed_nuclei map (x=> getNucleiPixels(green,x))
		val red_pixels = focussed_nuclei map (x=> getNucleiPixels(red,x))
		val nucleus_areas = focussed_nuclei.map(n=>getNucleusMeanArea(n))
		// val boundaries = focussed_nuclei map(x=> getNucleiBoundaries(x))
		val rg_r_values = pearsonsWrapper(green_pixels,red_pixels) map (x=>x.toString)
		val bg_r_values = pearsonsWrapper(blue_pixels,green_pixels)
		val rb_r_values = pearsonsWrapper(blue_pixels,red_pixels)
		val green_intensities = measureNucleiIntensities(green,focussed_nuclei) map (x => x.toString)
		val red_intensities = measureNucleiIntensities(red,focussed_nuclei) map (x => x.toString)
		val rows = for (i <-nucleus_areas.indices) yield (List(explanatory_value,nucleus_areas(i),green_intensities(i),red_intensities(i),rg_r_values(i),bg_r_values(i),rb_r_values(i),green_object_count(i),green_object_mean_area(i),green_object_sd_area(i),red_object_count(i),red_object_mean_area(i),red_object_sd_area(i)))
		for (r<-rows){
			println(r)
			csv_writer.writeRow(r)
		WindowManager.closeAllWindows()
		}
		
		
		

		}

	

	def main(args: Array[String]){

		val output_file = "Test.csv"
		val csv_writer = CSVWriter.open(output_file,append=true)
		csv_writer.writeRow(Seq("ExplanatoryValue","Nucleus Area","GreenIntensity","RedIntensity","GreenRedPearson","GreenBluePearson","RedBluePearson","GreenObjectCount","GreenObjectMeanArea","GreenObjectSD","RedObjectCount","RedObjectMeanArea","RedObjectSD"))
		val top_directory = new ij.io.DirectoryChooser("Choose Directory").getDirectory().toString
		// val top_directory = "/Users/work/Documents/h2ax_time_series_2/"
		for (subdirectory_name <- getListOfSubDirectories(top_directory)){
			
			for (file<-getListOfFilesInSubDirectory(top_directory+subdirectory_name)){
				println(top_directory+subdirectory_name+"/"+file)
				processImageFile(top_directory+subdirectory_name+"/"+file,csv_writer,subdirectory_name)
				}
			}


		}


}
