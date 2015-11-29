

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
import java.io._


class ThresholdMethodsComparison extends FunSuite with Checkers{
  
  val rand = new scala.util.Random(123456)
  val pw = new PrintWriter(new File("segmentation_comparison.txt" ))

  
  
  
  def noisePixels(template:Array[Array[Int]],foreground_mean:Int,background_mean:Int,noise:Int):Array[Array[Double]] = {
    val image_width = template.head.length
    val pixels = template.flatten
    val noised_pixels = pixels.map{
      p => (if (p == 1) foreground_mean else background_mean) + rand.nextGaussian * noise
    }.map(p=>p.toDouble)
    noised_pixels.grouped(image_width).toArray
  }
  
  def makeImage(image:Array[Array[Double]]):ij.ImagePlus = {
    val image_height =image.length
    val image_width = image.head.length
    val fimage = image.map{r=>r.map{p=>p.toFloat}}
    val processor = new ij.process.FloatProcessor(image_height,image_width)
    processor.setFloatArray(fimage)
    new ij.ImagePlus("synthetic image",processor)
  }
  
  def countMaskBlobs(image:Array[Array[Int]],centroid:(Double,Double)):Int = {
    Blobs.analyzePixelArray("test",image,"Test",centroid).getEntries.head.getValue match{
      case Some(x) => x.toInt
      case _ => 0
    }
  }
  
  def countForegroundPixels(image:Array[Array[Int]]):Int = {
    image.flatten.filter(x=> x!=0).size
  }
  
  
  val signalNoiseGen = for {
    bc<-(rand.nextGaussian()+1)/15000
    ep<-(rand.nextGaussian()+1)/10
    b<-Gen.choose(1,50)
    f<-Gen.choose(b+50,240)
    n<-Gen.choose(5,25)
  } yield (b,f,n,bc,ep)
  
  def scoreThreshold(threshold:Double,image:Array[Array[Double]],base_blob_count:Int,base_foreground_count:Int):String = {
    val thresholded_image = image.map{row=>row.map{pixel=>{
      if (pixel < threshold) 0
      else 255
    }}}
    val centroid = (image.head.length/2d,image.length/2d)
    val test_blob_count = countMaskBlobs(thresholded_image,centroid)
    val test_foreground_count = thresholded_image.flatten.filter(x=> x > 0).size
    val count_error = (base_blob_count - test_blob_count).toDouble / base_blob_count
    val foreground_error:Double = (base_foreground_count - test_foreground_count).toDouble / base_foreground_count
    count_error.toString + "," + foreground_error.toString
  } 
  

  
  def methodTester(b:Int,f:Int,n:Int,bc:Double,ec:Double):Boolean = {
    val test_base = Synthetics.createPixels(300,300,bc,ec)
    val centroid = (test_base.head.length/2d,test_base.length/2d)
    val base_blob_count = Synthetics.countMaskBlobs(test_base.map(r=>r.map(p=>255*p)),centroid)
    val base_foreground_count = Synthetics.countForegroundPixels(test_base)
    val noisy_image = Synthetics.noisePixels(test_base,f,b,n)
    val noisy_imageplus = Synthetics.makeImage(noisy_image)
    val noisy_pixels = noisy_image.flatten
    ImageIO.drawPixels(noisy_imageplus)
    
    val norm_pixels = Stats.standardScores(noisy_pixels).toList
    val five_means_threshold = ObjectThresholding.fiveMeans(norm_pixels,List(norm_pixels.min),List(Stats.mean(norm_pixels)/4),List(Stats.mean(norm_pixels)/2),List(Stats.mean(norm_pixels)),List(norm_pixels.max)) * Stats.standardDeviation(noisy_pixels)+Stats.mean(noisy_pixels)
    val four_means_threshold = ObjectThresholding.fourMeans(norm_pixels,List(norm_pixels.min),List(Stats.mean(norm_pixels)/3),List(Stats.mean(norm_pixels)/1.5),List(norm_pixels.max)) * Stats.standardDeviation(noisy_pixels)+Stats.mean(noisy_pixels)
    val five_means_scores = scoreThreshold(five_means_threshold,noisy_image,base_blob_count,base_foreground_count)
    val four_means_scores = scoreThreshold(four_means_threshold,noisy_image,base_blob_count,base_foreground_count)
    val three_means_threshold = ObjectThresholding.threeMeans(norm_pixels,List(-1d),List(0d),List(1d))
    val three_means_scores = scoreThreshold(three_means_threshold,noisy_image,base_blob_count,base_foreground_count)
    val two_means_threshold = ObjectThresholding.twoMeans(norm_pixels,List(-1d),List(1d))
    val two_means_scores = scoreThreshold(two_means_threshold,noisy_image,base_blob_count,base_foreground_count)
    val sct_threshold = ObjectThresholding.SCTWrapper(norm_pixels.toArray)* Stats.standardDeviation(noisy_pixels)+Stats.mean(noisy_pixels)
    val sct_scores = scoreThreshold(sct_threshold,noisy_image,base_blob_count,base_foreground_count)
    val output_string = List(base_blob_count.toString,b.toString ,f.toString , n.toString,five_means_scores,four_means_scores , three_means_scores,two_means_scores,sct_scores).mkString(",")
    println(output_string)
    pw.write(output_string)
    true
  }
  
  val checkthem = forAllNoShrink(signalNoiseGen){
    case (b,f,n,bc,ep) => methodTester(b,f,n,bc,ep)
  }
  
  checkthem.check
  pw.close()
}