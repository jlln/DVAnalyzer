

/**
 * @author james

 */
import ij.ImagePlus
import ij.plugin.ChannelSplitter
import ij.IJ
import ij.WindowManager
import ij.plugin.frame.RoiManager
import ij.measure.ResultsTable
import ij.plugin.filter.ParticleAnalyzer
import ij.plugin.filter.Analyzer

object NucleiProcessing {
  def processImageToNuclei(image:ij.ImagePlus):(List[Nucleus],List[ImagePlus],ImagePlus)={
    /*
     * A function to facilitate the processing of an image into a 
     * list of nuclei and separate channels.
     * It also aims to remove debris and poorly focussed nuclei by comparing nuclear pixel intensity
     * variances across the entire image.
     */
    val channels:List[ImagePlus]=ChannelSplitter.split(image).toList
    val blue = channels.last
    val (nuclei,nuclei_mask) = {maskNuclei(blue)}
    // val focussed_nuclei = threeChannelNucleiFocusser(red,green,blue,nuclei)
    val edge_mask = blue.duplicate
    IJ.run(edge_mask, "Find Edges","stack")
    val focussed_nuclei_and_variances = {nuclei.toList.map{n=>nucleiFocusser(n,edge_mask)}}
    val nuclear_variances = focussed_nuclei_and_variances.map(_._2)
    val image_nuclear_variance_mean = Stats.mean(nuclear_variances)
    val image_nuclear_variance_z_scores = Stats.standardScores(nuclear_variances)
    val retained_indices:List[Int] = nuclear_variances.zipWithIndex.filter{
      case (v,i) => {
        (v > -0.5) 
      }
    }.map(_._2)
    val focussed_nuclei = focussed_nuclei_and_variances.map(_._1)
    val retained_nuclei = retained_indices.map(i=>focussed_nuclei(i)).filter(n=> n.getSlices.length > 0)
    (retained_nuclei,channels,nuclei_mask)
  }

  def maskNuclei(blue:ImagePlus):(List[Nucleus],ImagePlus)={
    /*
     * Identifies the nuclei in the image using the DNA stained channel.
     * 
     */
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
    val nuclei:List[Nucleus] = processed_roi.tail.foldLeft(List(new Nucleus(List(processed_roi.head))))((n:List[Nucleus],ns:NucleusSlice) 
        => mergeNuclei(n,ns))

    
    (nuclei,nuclei_mask)
    }
  
  def mergeNuclei(nuclei:List[Nucleus],ro_i:NucleusSlice):List[Nucleus]={
    
  /**  If a slice is found to be from an existing nucleus, merge it into that nucleus, else create a new nucleus.
	*    How to discern if a slice belongs to a given nucleus? 
	*    First find other nuclei that are within the radius of the given nucleus,
	*    then from this set of nuclei (if there are more than one) choose the closest.
	*   Only the previous slice is considered.  
	*/
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
      nuclei :+ new Nucleus(List(ro_i))
    }    
  }
  
  def nucleiFocusser(nucleus:Nucleus,edge_mask:ij.ImagePlus):(Nucleus,Double) = {
    /*Identifies which slices are in focus for each nucleus
     * Measures the variance of the pixel intensity within each slice of an
     * edge-enhanced version of the DNA staining image for the nucleus.
     * Slices within 80% of the maximum variance value for the nucleus stack are retained.
     */
    var variance_results = new ResultsTable()
    val measurements = ij.measure.Measurements.MEAN+ij.measure.Measurements.AREA
    val analyzer= new Analyzer(edge_mask,measurements,variance_results)
    for (s<-nucleus.getSlices){
      edge_mask.setSlice(s.getSlice)
      edge_mask.setRoi(s.getRoi)
      analyzer.measure()
    }
    val slices = nucleus.getSlices
    val variance_values1 = variance_results.getColumn(variance_results.getColumnIndex("Mean"))
    val variance_values=variance_values1.map(x => x*x)
    val area_values = variance_results.getColumn(variance_results.getColumnIndex("Area"))
    val mean_area = Stats.mean(area_values)
    val variance_threshold = 0.8*variance_values.max        
    val retained_slice_indices = for ((s,i)<-nucleus.getSlices.zipWithIndex 
      if (variance_values(i)>variance_threshold & area_values(i)>mean_area*0.5)) yield{
        i
    }
    val retained_slices = retained_slice_indices.map(i=> slices(i))
    val retained_slice_variances = retained_slice_indices.map(i=>variance_values(i))
    val mean_variance = Stats.mean(retained_slice_variances)
    (new Nucleus(retained_slices),mean_variance)
  }
 
  def nucleusSliceEucledian(n1:NucleusSlice,n2:NucleusSlice):Double = {
    Stats.eucledian(n1.getXCentre,n1.getYCentre,n2.getXCentre,n2.getYCentre)
  }
    
    

    

}