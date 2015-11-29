

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

    val image_object_masks_thresholds = channels.map(c => ObjectThresholding.thresholdObjects(nucleus, c,masking_images))
    val image_object_masks = image_object_masks_thresholds.map(_._1)
    val thresholds = image_object_masks_thresholds.map(_._2)
    val nucleus_centroids = nucleus.getCentroids
    println("Analyzing subnuclear objects")
    val object_results = image_object_masks.zipWithIndex.map{
      case (c,i) => Blobs.analyzePixelArrayStack(condition,c, s"Channel${i+1}", nucleus_centroids)
    }
    
    val combined_object_results = Results.concatenateResultList(object_results)
    val mean_pixel_intensities = channels.map(c => nucleus.getPixels(c,masking_images).flatten.flatten).map(x => Stats.mean(x))
    val area = nucleus.getPixels(channels(0),masking_images).flatten.flatten.length
    val pixel_intensity_labels = channels.zipWithIndex.map{
      case (c,i) => s"MeanIntensityChannel${i+1}"
    }
    val pixel_intensity_result_entries = pixel_intensity_labels.zip(mean_pixel_intensities).map {
      case (l, m) => new ResultEntry(l, Some(m))
    }
    val intensity_result = new Result(condition,area, pixel_intensity_result_entries)
   
    
    val channel_data = channels.map{
      c=> nucleus.getPixels(c,masking_images).flatten.flatten.toList
    }.toList
    val object_masks = image_object_masks.map{
      c=> c.flatten.flatten.toList
    }
    println(channel_data.head.length,object_masks.head.length)
    
    val pearson_result = Colocalization.correlationPearson(condition,channel_data)
    val manders_result =Colocalization.manders(condition,object_masks) 
    Results.concatenateResultList(List(intensity_result,pearson_result,manders_result,combined_object_results))
  }
  
 
  
  def processImage(condition:String,imagepath: String): List[Result] = {
    val image:ij.ImagePlus = Profiling.timed(Profiling.printTime("Image loaded in")){ImageIO.openImageFile(imagepath)}
    val (nuclei, channels, mask) = NucleiProcessing.processImageToNuclei(image) //also divides the image into separate channels
    val nuclei_masks = nuclei.map(n=>n.getMaskingImages(mask))
    val nuclei_and_masks = nuclei.zip(nuclei_masks)
    nuclei_and_masks.map { 
      case (n,m) => {

        examineNucleus(condition,n, channels,m) }
      }
  }
  


  
  def main(args: Array[String]) {
    val top_directory = new ij.io.DirectoryChooser("Choose Directory").getDirectory().toString
    val output_file = top_directory + "FullZisaAnalysis.csv"
    val csv_writer = CSVWriter.open(output_file,append=false)
    var first = true
    val subdirectories = getListOfSubDirectories(top_directory)
    for (s<-subdirectories){
      for (f<-getListOfFilesInSubDirectory(top_directory+s)){
        val file_rows = processImage(s,top_directory+s+"/"+f)
        if (first){
          csv_writer.writeRow(file_rows.head.makeHeadings)
          first = false
        }
        file_rows.map(r=>csv_writer.writeRow(r.makeValueString))
      }
    }
    println("Done!")
  }

}
