

/**
 * @author james
 */
import ij._ 
import ij.plugin.filter.ParticleAnalyzer 

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._


import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest.prop.Checkers
class ThresholdChecker extends FunSuite with Checkers{

  val threshold_test_image_1 = "/home/james/workspace/Zisa/test/threshold_test_image_1.tif"
  val threshold_test_image_1_blob_count = 17
  val base_array_1 = Array(
      
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Array(0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Array(0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0),
  Array(0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0),
  Array(0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,1,1,0,0,0,0),
  Array(0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0),
  Array(0,0,1,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0),
  Array(0,0,1,1,1,1,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0),
  Array(0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Array(0,0,0,1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  )
  
  val count_1 = 5
  
  val base_array_2 = Array(
      
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Array(0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,1,0,0,0),
  Array(0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,1,1,1,0,0,0),
  Array(0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,1,0,1,0,0,0),
  Array(0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,1,1,1,0,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Array(0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Array(0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Array(0,0,1,1,1,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0),
  Array(0,0,1,1,1,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0),
  Array(0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0)
  )
  
  val count_2 = 5
  
  val base_array_3 = Array(
      
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Array(0,0,1,1,1,1,0,0,0,1,1,1,1,0,0,1,1,1,1,0,0),
  Array(0,0,1,1,1,1,0,0,0,1,1,1,1,0,0,1,1,1,1,0,0),
  Array(0,0,1,1,1,1,0,0,0,1,1,1,1,0,0,1,1,1,1,0,0),
  Array(0,0,1,1,1,1,0,0,0,1,1,1,1,0,0,1,1,1,1,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Array(0,0,1,1,1,1,0,0,0,1,1,1,1,0,0,1,1,1,1,0,0),
  Array(0,0,1,1,1,1,0,0,0,1,1,1,1,0,0,1,1,1,1,0,0),
  Array(0,0,1,1,1,1,0,0,0,1,1,1,1,0,0,1,1,1,1,0,0),
  Array(0,0,1,1,1,1,0,0,0,1,1,1,1,0,0,1,1,1,1,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Array(0,0,1,1,1,1,0,0,0,1,1,1,1,0,0,1,1,1,1,0,0),
  Array(0,0,1,1,1,1,0,0,0,1,1,1,1,0,0,1,1,1,1,0,0),
  Array(0,0,1,1,1,1,0,0,0,1,1,1,1,0,0,1,1,1,1,0,0),
  Array(0,0,1,1,1,1,0,0,0,1,1,1,1,0,0,1,1,1,1,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  )
  
  val count_3 = 9
  
  val base_array_4 = Array(
      
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Array(0,0,1,1,1,1,0,0,0,1,1,1,0,0,0,0,1,1,1,0,0),
  Array(0,0,1,1,0,1,0,0,0,1,1,1,0,0,0,1,0,1,1,0,0),
  Array(0,0,0,1,0,1,0,0,0,1,0,0,0,0,0,1,1,0,1,0,0),
  Array(0,0,1,1,0,1,0,0,0,1,1,1,1,0,0,1,1,1,0,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Array(0,0,1,1,1,1,0,0,0,1,0,0,1,0,0,1,1,0,1,0,0),
  Array(0,0,1,0,1,1,0,0,0,1,0,0,1,0,0,1,1,0,1,0,0),
  Array(0,0,1,0,0,1,0,0,0,1,1,1,1,0,0,1,0,1,1,0,0),
  Array(0,0,1,1,1,1,0,0,0,1,1,0,1,0,0,0,1,1,1,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Array(0,0,1,1,1,1,0,0,0,1,1,1,1,0,0,1,1,1,1,0,0),
  Array(0,0,1,1,1,1,0,0,0,1,1,1,1,0,0,1,1,1,1,0,0),
  Array(0,0,1,1,1,1,0,0,0,1,1,1,1,0,0,1,1,1,1,0,0),
  Array(0,0,1,1,1,1,0,0,0,1,1,1,1,0,0,1,1,1,1,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  )
  
  val count_4 = 11
  
  val rand = new scala.util.Random
  
  def noiseImage(image:Array[Array[Int]],mean_foreground:Int,mean_background:Int,noise:Int):Array[Array[Int]] = {
    val image_width = image.head.size
    val flat_array = image.flatten
    val noised_array:Array[Int] = flat_array.map{
      p=>{
        if (p==1) mean_foreground + (rand.nextGaussian().toInt * noise)
        else mean_background + (rand.nextGaussian().toInt * noise)
        
      }
    }
    val corrected_noised_array = noised_array.map{
      x=> {
        if (x < 0) 0
        else if  (x > 256) 256
        else x
      }
    }
    corrected_noised_array.grouped(image_width).toArray
  }
  
  
  def checkImage(pixels:Array[Array[Int]],n_objects:Int,mean_foreground:Int,mean_background:Int,noise:Int):Boolean={
    val noisy_pixels = noiseImage(pixels,mean_foreground,mean_background,noise)
    val image = ImageIO.makeImage(noisy_pixels)
    image.show()
    IJ.run("Tile")
    Thread.sleep(100)
    ij.IJ.run(image,"Make Binary", "method=Default background=Dark stack ")
    val image_width = pixels.head.length
    val image_height = pixels.length
    val image_area = image_width * image_height
    var roim= new ij.plugin.frame.RoiManager()
    var results_table= new ij.measure.ResultsTable()
    val pa = new ParticleAnalyzer(ParticleAnalyzer.ADD_TO_MANAGER,
          ij.measure.Measurements.MEAN+ij.measure.Measurements.CENTROID+ij.measure.Measurements.AREA,
          results_table,
          0.00,2000,
          0,1.0)
    pa.analyze(image)
    val areas_index = results_table.getColumnIndex("Area")
    val n_blobs = {
       if (areas_index== -1) 0
       else results_table.getColumn(areas_index).length
    }
    println(n_blobs)
    if (n_blobs == n_objects) true
    else false
  } 
  
  
    
 val propertyGen = for {
    b <- Gen.choose(1,150)
    f <-Gen.choose(b+20,256)
    n <-Gen.choose(1,(f-b)/10)
  } yield (b,f,n)
  

    


  val prop_check = forAllNoShrink(propertyGen){
    case (b,f,n) => {
      println(b,f,n)
      checkImage(base_array_2,count_2,f,b,n)
      }  
    }
  test("Image1"){
    check(prop_check)
  }
}  
  

  




