

/**
 * @author james
 */

import com.github.tototoshi.csv._
import java.io._
import ij._
object Zisa {

  def getListOfSubDirectories(directoryName: String): Array[String] = 
       (new File(directoryName)).listFiles.filter(_.isDirectory).map(_.getName)
  
  def getListOfFilesInSubDirectory(directoryName: String): Array[String] = 
       (new File(directoryName)).listFiles.map(_.getName).filter(_.contains(".tif"))
       
  def examineNucleus(condition:String,nucleus: Nucleus, channels: List[ij.ImagePlus],masking_images:List[List[List[Int]]]) = {

    val image_object_masks_thresholds = channels.map(c => ObjectThresholding.thresholdObjects(nucleus, c))
    val image_object_masks = image_object_masks_thresholds.map(_._1)
    val thresholds = image_object_masks_thresholds.map(_._2)
    val nucleus_centroids = nucleus.getCentroids
    println("Analyzing subnuclear objects")
    val object_results = image_object_masks.zipWithIndex.map{
      case (c,i) => Blobs.analyzePixelArrayStack(condition,c, s"Channel$i", nucleus_centroids)
    }
    
    val combined_object_results = Results.concatenateResultList(object_results)
    val mean_pixel_intensities = channels.map(c => nucleus.getPixels(c).flatten.flatten).map(x => Stats.mean(x))
    val area = nucleus.getPixels(channels(0)).flatten.flatten.length
    val pixel_intensity_labels = List("MeanRedIntensity", "MeanGreenIntensity", "MeanBlueIntensity")
    val pixel_intensity_result_entries = pixel_intensity_labels.zip(mean_pixel_intensities).map {
      case (l, m) => new ResultEntry(l, Some(m))
    }
    val intensity_result = new Result(condition,area, pixel_intensity_result_entries)
   
    val red = nucleus.getPixels(channels(0)).map(x=>x.toList.map(y=>y.toList)).toList
    val green =  nucleus.getPixels(channels(1)).map(x=>x.toList.map(y=>y.toList)).toList
    val blue =  nucleus.getPixels(channels(2)).map(x=>x.toList.map(y=>y.toList)).toList
    val cdt = Colocalization.constrainedDisplacementTesting(red,green,blue,masking_images)
    val manders_result =Colocalization.threeWayManders(image_object_masks(0).flatten.flatten, image_object_masks(1).flatten.flatten, image_object_masks(2).flatten.flatten) 
    val r =Results.concatenateResultList(List(combined_object_results,manders_result,cdt))
    r
  }
  
 
  
  def processImage(condition:String,imagepath: String): List[List[String]] = {
    val image:ij.ImagePlus = Profiling.timed(Profiling.printTime("Image loaded in")){ImageIO.openImageFile(imagepath)}
    val (nuclei, channels, mask) = NucleiProcessing.processImageToNuclei(image) //also divides the image into separate channels
    val nuclei_masks = nuclei.map(n=>n.getMaskingImages(mask))
    val nuclei_and_masks = nuclei.zip(nuclei_masks)
    nuclei_and_masks.map { 
      case (n,m) => {

        examineNucleus(condition,n, channels,m).makeValueString }
      }
  }
  


  
  def main(args: Array[String]) {
    val top_directory = new ij.io.DirectoryChooser("Choose Directory").getDirectory().toString
    val output_file = top_directory + "FullZisaAnalysis.csv"
    val csv_writer = CSVWriter.open(output_file,append=false)
    val headers = List("ExperimentalCondition","TotalNucleusArea","RG_coloc_p_value", "RB_coloc_p_value", "BG coloc_p_value", "RG_pearsons_r", "RB_pearsons_r", "BG_pearsons_r","GreenOverlapRed","RedOverlapGreen","RedOverlapBlue","BlueOverlapRed","GreenOverlapBlue","BlueOverlapGreen","MeanRedIntensity","MeanGreenIntensity","MeanBlueIntensity","RedBlobCount","RedMeanBlobSize","RedSDBlobSize","RedSkewnessBlobSize","RedKurtosisBlobSize","RedMeanRadialBlobDistance","RedSDRadialBlobDistance","RedSkewnessRadialBlobDistance","RedKurtosisRadialBlobDistance","RedMeanNearestNeighbour","RedSDNearestNeigbour","RedSkewnessNearestNeighbour","RedKurtosisNearestNeighbour","GreenBlobCount","GreenMeanBlobSize","GreenSDBlobSize","GreenSkewnessBlobSize","GreenKurtosisBlobSize","GreenMeanRadialBlobDistance","GreenSDRadialBlobDistance","GreenSkewnessRadialBlobDistance","GreenKurtosisRadialBlobDistance","GreenMeanNearestNeighbour","GreenSDNearestNeigbour","GreenSkewnessNearestNeighbour","GreenKurtosisNearestNeighbour","BlueBlobCount","BlueMeanBlobSize","BlueSDBlobSize","BlueSkewnessBlobSize","BlueKurtosisBlobSize","BlueMeanRadialBlobDistance","BlueSDRadialBlobDistance","BlueSkewnessRadialBlobDistance","BlueKurtosisRadialBlobDistance","BlueMeanNearestNeighbour","BlueSDNearestNeigbour","BlueSkewnessNearestNeighbour","BlueKurtosisNearestNeighbour")
    csv_writer.writeRow(headers.toSeq)
    val subdirectories = getListOfSubDirectories(top_directory)
    for (s<-subdirectories){
      for (f<-getListOfFilesInSubDirectory(top_directory+s)){
        val file_rows = processImage(s,top_directory+s+"/"+f)
//        file_rows.foreach(println)
        file_rows.map(r=>csv_writer.writeRow(r))
      }
    }
    println("Done!")
  }

}
