

/**
 * @author james
 */
import ij._
import ij.plugin.filter.ParticleAnalyzer


object Blobs{
  
  def nearestNeighbours(object_centroids:List[(Float,Float)]):List[Double] = {
    object_centroids.length match{
      case 0 => List(0)
      case 1 => List(0)
      case _ => object_centroids.map{
        c => {
          val other_points = object_centroids.filter{x=> x!= c}
          val distances = other_points.map{
            case (x,y) => {Stats.eucledian(c._1,c._2,x,y)}
          }
          distances.min
        }
      }
    }
  }
  
  def measurementStats[T](items:Traversable[T])(implicit n:Numeric[T]) : List[Option[Double]] = {
    val stats = List(Stats.mean(items),Stats.standardDeviation(items),Stats.skewness(items),Stats.kurtosis(items))
    stats.map{x=> x match{
      case x if x.isNaN => None
      case x => Some(x)
    }}
  }
  
  
  def analyzePixelArray(condition:String,pixels:Array[Array[Int]],colour:String,nuclear_slice_centroid:(Double,Double)):Result = {

    val image = ImageIO.makeImage(pixels)

    ij.IJ.run(image,"Make Binary", "method=Default background=Dark stack ")
    val image_width = pixels.head.length
    val image_height = pixels.length
    val image_area = image_width * image_height
    var roim= new ij.plugin.frame.RoiManager()
    var results_table= new ij.measure.ResultsTable()
    val pa = new ParticleAnalyzer(ParticleAnalyzer.ADD_TO_MANAGER,
          ij.measure.Measurements.MEAN+ij.measure.Measurements.CENTROID+ij.measure.Measurements.AREA,
          results_table,
          1,20000,
          0,1.0)
    pa.analyze(image)
    val areas_index = results_table.getColumnIndex("Area")
    val labels = List("BlobCount","MeanBlobSize","SDBlobSize","SkewnessBlobSize","KurtosisBlobSize",
       "MeanRadialBlobDistance","SDRadialBlobDistance","SkewnessRadialBlobDistance","KurtosisRadialBlobDistance",
       "MeanNearestNeighbour","SDNearestNeigbour","SkewnessNearestNeighbour","KurtosisNearestNeighbour").map{x=>colour+x}
    if (areas_index== -1){
      val result_entries = labels.map(l=>new ResultEntry(l,None)).updated(0,new ResultEntry(colour+"BlobCount",Some(0)))
      new Result(condition,image_area,result_entries)
    }
    else {
     
      val areas:Array[Float] = results_table.getColumn(areas_index)
      val x_centres:Array[Float] = results_table.getColumn(results_table.getColumnIndex("X"))
      val y_centres:Array[Float] = results_table.getColumn(results_table.getColumnIndex("Y"))
      val centroids = x_centres.zip(y_centres)
      val nearest_neighbours = nearestNeighbours(centroids.toList)
      val slice_centre_x:Double = nuclear_slice_centroid._1
      val slice_centre_y:Double = nuclear_slice_centroid._2
      val radiality:Seq[Double] = centroids.map{
        case (x,y) => (Stats.eucledian(x,y,slice_centre_x,slice_centre_y))/image_area
      }
      val areas_stats = measurementStats(areas)
      val nearest_neighbour_stats = measurementStats(nearest_neighbours)
      val radiality_stats = measurementStats(radiality)
      val blob_count = Some(areas.length.toDouble)
      val result_entry_values:List[Option[Double]] = List(blob_count) ++ areas_stats ++ 
          radiality_stats ++ nearest_neighbour_stats
      val result_entries = labels.zip(result_entry_values).map{
        case (l,v) => new ResultEntry(l,v)
      }
      
      WindowManager.closeAllWindows()
      new Result(condition,image_area,result_entries)
    }
  }
  def analyzePixelArrayStack(condition:String,pixels:Array[Array[Array[Int]]],colour:String,centroids:List[(Double,Double)]):Result = Profiling.timed(Profiling.printTime("Pixel blob array stack analysis completed in")){
    val r = Results.mergeResults(pixels.toList.zip(centroids).map{
      case (s,c)=>analyzePixelArray(condition,s,colour,c)
      })
    r
  }
  

  
  
}