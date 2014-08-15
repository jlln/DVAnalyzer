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
import breeze.linalg._
import breeze.stats.meanAndVariance


object findCells 
{ 


  


  def getListOfSubDirectories(directoryName: String): Array[String] = 
  {
    return (new File(directoryName)).listFiles.filter(_.isDirectory).map(_.getName)
  }

  def getListOfFilesInSubDirectory(directoryName: String): Array[String] = 
  {
    return (new File(directoryName)).listFiles.map(_.getName).filter(_.contains(".tif"))
  }

  def mean[T]( ts: Iterable[T] )( implicit num: Numeric[T] ) = 
  {
    num.toDouble( ts.sum ) / ts.size
  }

  



    
  
    case class CellSlice(slice:Int,x_centre:Double,y_centre:Double,roi:ij.gui.Roi,area:Double)
    case class Cell(slices:ListBuffer[CellSlice])
    case class ProcessedRoi(slice:Int,x_centre:Double,y_centre:Double,roi:ij.gui.Roi,area:Double)
    

    

    // Identifies focussed slices within the cell/nucleus.
    def cellFocusser(cell:Cell,edge_mask:ij.ImagePlus):Cell=
    {
        var variance_results = new ResultsTable()
        val measurements = ij.measure.Measurements.MEAN+ij.measure.Measurements.AREA
        val analyzer= new Analyzer(edge_mask,measurements,variance_results)
        for (s<-cell.slices){
          edge_mask.setSlice(s.slice)
          edge_mask.setRoi(s.roi)
          analyzer.measure()
        }
        val variance_values1 = variance_results.getColumn(variance_results.getColumnIndex("Mean"))
        val variance_values=variance_values1.map(x => x*x)
        val area_values = variance_results.getColumn(variance_results.getColumnIndex("Area"))
        
        val mean_area = mean(area_values)
        val variance_threshold = 0.7*variance_values.max        
        
        val retained_slices = for ((s,i)<-cell.slices.zipWithIndex if (variance_values(i)>variance_threshold & area_values(i)>mean_area*0.8)) yield s
        
        return new Cell(retained_slices)
        
        

    }
    

    // Tracks nuclei through the z-stack, 
    def mergeCells(cells:ListBuffer[Cell],ro_i:ProcessedRoi):ListBuffer[Cell]={
      val slice = ro_i.slice
      val area = ro_i.area
      val radius = (scala.math.sqrt(area)/3.14156)
      val area_upper_limit = area*1.3
      val area_lower_limit = area*0.7
      val x = ro_i.x_centre
      val y = ro_i.y_centre
      for (c<-cells)
        {
          val last_slice = c.slices.last
          if (last_slice.x_centre > (x-radius) & last_slice.x_centre < (x+radius) 
            & last_slice.y_centre>(y-radius) & last_slice.y_centre < (y+radius) 
            & last_slice.area > area_lower_limit & last_slice.area < area_upper_limit)
          {
            c.slices.append(CellSlice(ro_i.slice,ro_i.x_centre,ro_i.y_centre,ro_i.roi,ro_i.area))
            return cells
          }
          
        }
      val new_cell = Cell(ListBuffer(CellSlice(ro_i.slice,ro_i.x_centre,ro_i.y_centre,ro_i.roi,ro_i.area)))
      var new_cells:ListBuffer[Cell]=cells
      cells.append(new_cell)
    
      return new_cells
      
    } 






    def median[T](s: Seq[T])(implicit n: Fractional[T]) = 
    {
      import n._
      val (lower, upper) = s.sortWith(_<_).splitAt(s.size / 2)
      if (s.size % 2 == 0) (lower.last + upper.head) / fromInt(2) else upper.head
    }


    def splitChannels(image:ImagePlus):Array[ImagePlus]=
  {
    val channels=ChannelSplitter.split(image)
    return channels  
  }
  

// Identifies the nuclei, and then for each nuclei identifies the slices that are in focus.
    def prepareCells(blue:ij.ImagePlus):ListBuffer[Cell]={
      val nuclei_mask=blue.duplicate()
      nuclei_mask.getChannelProcessor().resetMinAndMax()
      println("Preparing nuclei mask")
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

     

      for (i<-(0 until nuclei_mask.getStackSize()))
        {
          nuclei_mask.setSliceWithoutUpdate(i + 1)
          pa.analyze(nuclei_mask)
        }

      val rm=roim.getRoisAsArray()
      val slice_list= for (roi<-rm) yield roi.getName().split("-")(0).toInt
      val roi_and_slice = (rm.zip(slice_list)).sortBy(_._2)  
     
      val x_centres = results.getColumn(results.getColumnIndex("X")).toList
      val y_centres = results.getColumn(results.getColumnIndex("Y")).toList
      val areas = results.getColumn(results.getColumnIndex("Area")).toList


      
    
      

      val processed_roi = ((((slice_list zip x_centres) zip y_centres) zip rm) zip areas) map 
      {
        case ((((s,x),y),r),sr) => ProcessedRoi(s,x,y,r,sr)
      }

      
      

      var cells:ListBuffer[Cell]=ListBuffer()
      for (r <- processed_roi)
      {
        cells=mergeCells(cells,r)
        
      }
      

      val edge_mask = blue.duplicate()
      IJ.run(edge_mask, "Find Edges","stack")

      
      cells = for (c<-cells if c.slices.length >9) yield c
      val focussed_cells = for (c<-cells) yield cellFocusser(c,edge_mask)

      WindowManager.closeAllWindows()
      return focussed_cells
    }
      

    // Returns the average intensity for the nuclei across all of its slices.
    def nucleiIntensityMean(image:ij.ImagePlus,nucleiRoiList:ListBuffer[CellSlice]):Double={
      val measurements=ij.measure.Measurements.MEAN+ij.measure.Measurements.AREA
      var results= new ResultsTable()
      val analyzer = new Analyzer(image, measurements, results)
      
      for (slice<-nucleiRoiList){
        image.setSlice(slice.slice)
        image.setRoi(slice.roi)
        image.getChannelProcessor().resetMinAndMax()
        analyzer.measure()
      }

      val m_i=results.getColumnIndex("Mean")
      val areas = results.getColumn(results.getColumnIndex("Area"))
      val intensities=results.getColumn(results.getColumnIndex("Mean"))
      val area_corrected_values = intensities.zip(areas) map { case (i,a)=>i/a}
      val area_corrected_mean = ((intensities zip areas) map {case (i,a)=>i*a}).sum/(areas.sum)
      return area_corrected_mean
      
    }

    
    

    // Measures intensity for each channel in the specified nuclei
    def processNuclei(nuclei:Cell,image:ij.ImagePlus):List[Double]={
      val channels=splitChannels(image).toList
      val row = for (c<-channels) yield nucleiIntensityMean(c,nuclei.slices)
      return row

    }

// Iterates over the nuclei slices, projecting overlays onto the specified image. 
// It handles stacks, but due to the slow speed of image rendering in scala it wont get every slice.
    def visualCheck(image:ij.ImagePlus,nuclei:Cell){
      
      println(nuclei.slices.length)
      
      for (s<-nuclei.slices){
        image.setSlice(s.slice)
        val rectangle_roi = new ij.gui.Roi(s.roi.getBounds())
        image.setRoi(rectangle_roi)
        val cropped_image = image.getProcessor.crop()
        val preview_image = new ij.ImagePlus("i",cropped_image)
        preview_image.getProcessor.resetMinAndMax()
        preview_image.show()
        Thread.sleep(100)
        WindowManager.closeAllWindows()
      }
    }
    

    // Gets the pixel values for the passed channel within the passed nuclei.
    def getNucleiPixels(image:ij.ImagePlus,nuclei:Cell):List[Array[Short]]={
      var pixel_array_by_slice:ListBuffer[Array[Short]] = ListBuffer()
      for (s<-nuclei.slices){
        image.setSlice(s.slice)
        val rectangle_roi = new ij.gui.Roi(s.roi.getBounds())
        image.setRoi(rectangle_roi)
        val cropped_image = image.getProcessor.crop()
        
        val pixels_array:Array[Short] = cropped_image.getPixels().asInstanceOf[Array[Short]]
        pixel_array_by_slice.append(pixels_array)
        }
        
       
        
        


      return pixel_array_by_slice.toList
    }


    def countParticles(image:ij.ImagePlus,nuclei:Cell):(Double,Double)={
      
      var areas_size:ListBuffer[Double]=ListBuffer()
      val n_particles:ListBuffer[Int]=ListBuffer()
      for (s<-nuclei.slices){
        var results = new ResultsTable()
        val pa = new ParticleAnalyzer(ParticleAnalyzer.ADD_TO_MANAGER,
          ij.measure.Measurements.AREA,
          results,
          1,5000000,
          0,1.0)
        image.setSlice(s.slice)
        val rectangle_roi = new ij.gui.Roi(s.roi.getBounds())
        image.setRoi(rectangle_roi)
        val cropped_image = new ij.ImagePlus(" ",image.getProcessor.crop())
        IJ.run(cropped_image, "Make Binary", "method=Otsu background=Dark")
        cropped_image.show()
        Thread.sleep(5)
        pa.analyze(cropped_image)
        val areas = results.getColumn(results.getColumnIndex("Area"))
        val mean_area = mean(areas)
        areas_size.append(mean_area)
        val n_areas = areas.length
        n_particles.append(n_areas)
        WindowManager.closeAllWindows()
      }
      return (mean(n_particles),mean(areas_size))

    }




//  Creates a stack of images masking the provided nuclei
    def roiMasker(image:ij.ImagePlus,nuclei:Cell):ij.ImagePlus={
      val nuclei_roi_mask_stack = new ij.ImageStack(image.getWidth(),image.getHeight())
      for (s<-nuclei.slices){
        var mask_slice = new ByteProcessor(image.getWidth(),image.getHeight())
          image.setSlice(s.slice)
          mask_slice.insert(s.roi.getMask(),s.roi.getBounds().x,s.roi.getBounds().y)
          nuclei_roi_mask_stack.addSlice(mask_slice)
          
      }
      val nuclei_roi_mask = new ImagePlus("Nuclei roi mask",nuclei_roi_mask_stack)
      
      return nuclei_roi_mask
    }


    def overUnder(x:Short,x_mean:Double) = if (x < x_mean) 0 else 1
    
    def differenceCounter(value:Int):Int = {
      if (value ==0) return 1
      else return 0
    }


    

    def intensityCorrelationQuotientSlice(channel_a:Array[Short],channel_b:Array[Short]):Float={
      val n_pixels = channel_a.length
      var a_mean:Float = 0
      channel_a.foreach(a_mean+=_)
      a_mean=a_mean/n_pixels
      var b_mean:Float = 0
      channel_b.foreach(b_mean+=_)
      b_mean=b_mean/n_pixels
      
      val channel_a_over_under = channel_a.map(a=>overUnder(a,a_mean))
      val channel_b_over_under = channel_b.map(b=>overUnder(b,b_mean))
      val difference = for ((a,b)<-channel_a_over_under.zip(channel_b_over_under)) yield a-b
      val scores = difference.map(a=>differenceCounter(a))
      var sum_scores = 0
       scores.foreach(sum_scores +=_)
      return (sum_scores.toFloat/n_pixels)
      
    }

    def intensityCorrelationQuotient(channel_a:List[Array[Short]],channel_b:List[Array[Short]]):Double={
      val results = for (i<-channel_a.indices) yield intensityCorrelationQuotientSlice(channel_a(i),channel_b(i))
      return mean(results)
    }


    def pearsonCorrelation(channel_a:List[Array[Short]],channel_b:List[Array[Short]]):Double =  {
      val r_values:ListBuffer[Double] = ListBuffer()
      for (i<-channel_a.indices){
        val v_a:DenseVector[Double] = DenseVector(channel_a(i).map(_.toDouble))
        val v_b:DenseVector[Double] = DenseVector(channel_b(i).map(_.toDouble))
        val a_mv = meanAndVariance(v_a)
        val b_mv = meanAndVariance(v_b)
        val a_mean = a_mv.mean
        val b_mean = b_mv.mean

        val a_sd = math.sqrt(a_mv.variance)
        val b_sd = math.sqrt(b_mv.variance)
        val n = channel_a(i).length
        val r = 1.0 / (n - 1.0) * sum( ((v_a - a_mean) / a_sd) :* ((v_b - b_mean) / b_sd) )
        r_values.append(r)
        }
      return mean(r_values)


    }


    def subtractBackground(image:ij.ImagePlus,radius:Int):ImagePlus={
      for (i<-(1 until image.getStackSize)){
        image.setSlice(i)
        val str = "rolling="+radius.toString+" , slice"
        // println(str)
        IJ.run(image,"Subtract Background...",str)
      }

      return image

    }





// Wrapper function to facilitate incorporation of explanatory variables from the directory structure
    def processTimePoint(directory:String,writer:CSVWriter){
      val timepoint=directory.toFloat
      val files=getListOfFilesInSubDirectory("/Users/work/Desktop/pH2AX Time Series/"+directory)
      println(timepoint)
      var opener= new ij.io.Opener()
      for (f<-files)
      {
        val path="/Users/work/Desktop/pH2AX Time Series/"+directory+"/"+f
        println(path)
        val imp=opener.openImage(path)
        println("image opened")
        
        println("Subtracting Background")
        var channels=splitChannels(imp)
        var blue=channels(0)
        var green=channels(1)
        green =subtractBackground(green,15)
        var red=channels(2)
        red = subtractBackground(red,15)
        val nuclei=prepareCells(blue)
        
        
        for (n<-nuclei){
          visualCheck(red,n)
          visualCheck(green,n)
          println("Analyzing")
          val pixel_g=getNucleiPixels(green,n)
          val pixel_r=getNucleiPixels(red,n)
          val pearson = pearsonCorrelation(pixel_r,pixel_g)
          println(pearson)
          
          val icq = intensityCorrelationQuotient(pixel_g,pixel_r)

          
          val row=processNuclei(n,imp)
          

          val line = Seq(timepoint)++row++Seq(icq,pearson)
          println(line)
          writer.writeRow(line)

        }

        
        
      }

   } 

  


  
  def main(args: Array[String]) 
  {

    val f = new File("pearson_icq_intensities.csv")
    val writer = CSVWriter.open(f,append=false)
    writer.writeRow(Seq("Time","Blue Intensity","Green Intensity","Red Intensity","Green Red Intensity Correlation Quotient,Green Red Pearson"))
    val directory = new ij.io.DirectoryChooser("Choose Directory").getDirectory()
    for (d<-getListOfSubDirectories("/Users/work/Desktop/pH2AX Time Series/")){
      
      processTimePoint(d,writer)
    }

    
    
    
    
  

    writer.close()

 
  }
}
