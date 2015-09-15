

/**
 * @author james
 */

import com.github.tototoshi.csv._
import java.io._
object Zisa {

  def getListOfSubDirectories(directoryName: String): Array[String] = 
       (new File(directoryName)).listFiles.filter(_.isDirectory).map(_.getName)
  
  def getListOfFilesInSubDirectory(directoryName: String): Array[String] = 
       (new File(directoryName)).listFiles.map(_.getName).filter(_.contains(".tif"))
       
  def examineNucleus(nucleus: Nucleus, channels: Array[ij.ImagePlus]) = {
    val image_object_masks_thresholds = channels.map(c => ObjectThresholding.thresholdObjects(nucleus, c))
    val image_object_masks = image_object_masks_thresholds.map(_._1)
    val thresholds = image_object_masks_thresholds.map(_._2)
    println("Analyzing subnuclear objects")
    val object_results_red: Result = Blobs.analyzePixelArrayStack(image_object_masks(0), "Red")
    val object_results_green: Result = Blobs.analyzePixelArrayStack(image_object_masks(1), "Green")
    val object_results_blue: Result = Blobs.analyzePixelArrayStack(image_object_masks(2), "Blue")
    val object_results = Results.concatenateResults(object_results_red, Results.concatenateResults(object_results_green, object_results_blue))
    val mean_pixel_intensities = channels.map(c => nucleus.getPixels(c).flatten.flatten).map(x => Stats.mean(x))
    val area = nucleus.getPixels(channels(0)).flatten.flatten.length
    val pixel_intensity_labels = List("MeanRedIntensity", "MeanGreenIntensity", "MeanBlueIntensity")
    val pixel_intensity_result_entries = pixel_intensity_labels.zip(mean_pixel_intensities).map {
      case (l, m) => new ResultEntry(l, Some(m))
    }
    val intensity_result = new Result(area, pixel_intensity_result_entries)
    val red_pixels_t = nucleus.getPixels(channels(0)).flatten.flatten.map {
      case x if x > thresholds(0) => x
      case _                      => 0
    }
    val green_pixels_t = nucleus.getPixels(channels(1)).flatten.flatten.map {
      case x if x > thresholds(0) => x
      case _                      => 0
    }
    val blue_pixels_t = nucleus.getPixels(channels(2)).flatten.flatten.map {
      case x if x > thresholds(0) => x
      case _                      => 0
    }
    val colocalization_result = Colocalization.threeWayPearson(red_pixels_t, green_pixels_t, blue_pixels_t)
    val r =Results.concatenateResults(colocalization_result, Results.concatenateResults(intensity_result, object_results))
    r
  }

  
  def processImage(condition:String,imagepath: String): List[List[String]] = {
    val image: ij.ImagePlus = ImageIO.openImageFile(imagepath)
    val (nuclei, channels) = NucleiProcessing.processImageToNuclei(image) //also divides the image into separate channels
    nuclei.map { n => condition +: examineNucleus(n, channels).makeValueString }
  }
  


  
  def main(args: Array[String]) {
    val top_directory = new ij.io.DirectoryChooser("Choose Directory").getDirectory().toString
    val output_file = top_directory + "FullZisaAnalysis.csv"
    val csv_writer = CSVWriter.open(output_file,append=false)
    val headers = List("ExperimentalCondition","RedGreenPearson","GreenBluePearson","RedBluePearson","MeanRedIntensity","MeanGreenIntensity","MeanBlueIntensity","RedBlobCount","RedMeanBlobSize","RedSDBlobSize","RedSkewnessBlobSize","RedKurtosisBlobSize","RedMeanRadialBlobDistance","RedSDRadialBlobDistance","RedSkewnessRadialBlobDistance","RedKurtosisRadialBlobDistance","RedMeanNearestNeighbour","RedSDNearestNeigbour","RedSkewnessNearestNeighbour","RedKurtosisNearestNeighbour","GreenBlobCount","GreenMeanBlobSize","GreenSDBlobSize","GreenSkewnessBlobSize","GreenKurtosisBlobSize","GreenMeanRadialBlobDistance","GreenSDRadialBlobDistance","GreenSkewnessRadialBlobDistance","GreenKurtosisRadialBlobDistance","GreenMeanNearestNeighbour","GreenSDNearestNeigbour","GreenSkewnessNearestNeighbour","GreenKurtosisNearestNeighbour","BlueBlobCount","BlueMeanBlobSize","BlueSDBlobSize","BlueSkewnessBlobSize","BlueKurtosisBlobSize","BlueMeanRadialBlobDistance","BlueSDRadialBlobDistance","BlueSkewnessRadialBlobDistance","BlueKurtosisRadialBlobDistance","BlueMeanNearestNeighbour","BlueSDNearestNeigbour","BlueSkewnessNearestNeighbour","BlueKurtosisNearestNeighbour")
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
