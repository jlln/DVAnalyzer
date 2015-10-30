

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
  

  
  def createPixels(x:Int,y:Int):Array[Array[Int]] = {
    val image = Array.fill(y){Array.fill(x){0}}
    def pixelIter(ix:Int,iy:Int,image:Array[Array[Int]]):Array[Array[Int]] = {
      val neighbours = (ix,iy) match {
        case (0,0) => List()
        case (0,iiy) => List(image(iiy-1)(0))
        case (iix,0) => List(image(0)(iix-1))
        case (iix,iiy) if iix < x-1 && iiy < y-1 => List(image(iiy)(iix-1),image(iiy-1)(ix),image(iiy-1)(ix+1))
        case (_,_) => List()
      }
      val cutoff = 0.008+neighbours.sum * 0.44
      val pixel = if (rand.nextFloat < cutoff) 1 else 0
      image(iy).update(ix,pixel)
      image
    }
    (0 until x).map{
      xx=>{
        (0 until y).map{
          yy=> pixelIter(xx,yy,image)
        }
      }
    }
    image
  }
  
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
    Blobs.analyzePixelArray(image,"Test",centroid).getEntries.head.getValue match{
      case Some(x) => x.toInt
      case _ => 0
    }
  }
  
  def countForegroundPixels(image:Array[Array[Int]]):Int = {
    image.flatten.filter(x=> x!=0).size
  }
  
  
  val signalNoiseGen = for {
    b<-Gen.choose(1,50)
    f<-Gen.choose(b+110,240)
    n<-Gen.choose(5,20)
  } yield (b,f,n)
  
  
  
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
    val test_blob_count = countMaskBlobs(thresholded_image,centroid)
    val test_foreground_count = countForegroundPixels(thresholded_image)
    val blob_error_rate:Double = (base_blob_count - test_blob_count).toFloat / base_blob_count
    val foreground_error_rate:Double = (base_foreground_count - test_foreground_count).toFloat / base_foreground_count
    println(blob_error_rate)
    println(foreground_error_rate)
    (scala.math.abs(blob_error_rate) < 0.05 && scala.math.abs(foreground_error_rate) < 0.05)
  }
  
  def thresholdExplorer(b:Int,f:Int,n:Int):Boolean = {

    val test_base = createPixels(300,300)
    val centroid = (test_base.head.length/2d,test_base.length/2d)
    val base_blob_count = countMaskBlobs(test_base.map(r=>r.map(p=>255*p)),centroid)
    val base_foreground_count = countForegroundPixels(test_base)
    val noisy_image = noisePixels(test_base,f,b,n)
    val noisy_imageplus = makeImage(noisy_image)
    val noisy_pixels = noisy_image.flatten

    ImageIO.drawPixels(noisy_imageplus)

    checker(base_blob_count,base_foreground_count,noisy_image,List((0d,1d)))
    
    
  }
  
  val propConsistentCounts = forAllNoShrink(signalNoiseGen){
    case (b,f,n) => thresholdExplorer(b,f,n)
  }
  propConsistentCounts.check

}  
  

  




