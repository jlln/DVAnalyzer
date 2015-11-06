

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
  
  val rand = new scala.util.Random(123456)

  def checker (base_blob_count:Int,base_foreground_count:Int,noisy_image:Array[Array[Double]],history:List[(Double,Double)]):Boolean = {
    
    val noisy_pixels = noisy_image.flatten
    val max_coeff = noisy_pixels.max
    val pixel_mean = Stats.mean(noisy_pixels)
    val standard_pixels = Stats.standardScores(noisy_pixels).toList
    val increment = (standard_pixels.max - standard_pixels.min)/200
    val threshold = ObjectThresholding.fourMeans(standard_pixels,List(standard_pixels.min),List(Stats.mean(standard_pixels)/3),List(Stats.mean(standard_pixels)/1.5),List(standard_pixels.max)) * Stats.standardDeviation(noisy_pixels) + pixel_mean
    val thresholded_image = noisy_image.map{row=>row.map{pixel=>{
      if (pixel < threshold) 0
      else 255
    }}}
    ImageIO.drawPixels(ImageIO.makeImage(thresholded_image))
    val centroid = (noisy_image.head.length/2d,noisy_image.length/2d)
    val test_blob_count = Synthetics.countMaskBlobs(thresholded_image,centroid)
    val test_foreground_count = Synthetics.countForegroundPixels(thresholded_image)
    val blob_error_rate:Double = (base_blob_count - test_blob_count).toFloat / base_blob_count
    val foreground_error_rate:Double = (base_foreground_count - test_foreground_count).toFloat / base_foreground_count
    println(blob_error_rate)
    println(foreground_error_rate)
    (scala.math.abs(blob_error_rate) < 0.1 && scala.math.abs(foreground_error_rate) < 0.05)
  }
  
  def thresholdExplorer(b:Int,f:Int,n:Int):Boolean = {

    val test_base = Synthetics.createPixels(300,300)
    val centroid = (test_base.head.length/2d,test_base.length/2d)
    val base_blob_count = Synthetics.countMaskBlobs(test_base.map(r=>r.map(p=>255*p)),centroid)
    val base_foreground_count = Synthetics.countForegroundPixels(test_base)
    val noisy_image = Synthetics.noisePixels(test_base,f,b,n)
    val noisy_imageplus = Synthetics.makeImage(noisy_image)
    val noisy_pixels = noisy_image.flatten

    ImageIO.drawPixels(noisy_imageplus)

    checker(base_blob_count,base_foreground_count,noisy_image,List((0d,1d)))
    
    
  }
  
  val propConsistentCounts = forAllNoShrink(Synthetics.signalNoiseGen){
    case (b,f,n) => thresholdExplorer(b,f,n)
  }
  propConsistentCounts.check

}  
  

  




