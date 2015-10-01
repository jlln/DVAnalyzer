

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
  
  case class Image(pixels:Array[Array[Float]])
  
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
      val cutoff = 0.007+neighbours.sum * 0.45
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
  
  def noisePixels(template:Array[Array[Int]],foreground_mean:Int,background_mean:Int,noise:Int):Array[Array[Float]] = {
    val image_width = template.head.length
    val pixels = template.flatten
    val noised_pixels = pixels.map{
      p => (if (p == 1) foreground_mean else background_mean) + rand.nextGaussian * noise
    }.map(p=>p.toFloat)
    noised_pixels.grouped(image_width).toArray
  }
  
  def makeImage(image:Array[Array[Float]]):ij.ImagePlus = {
    val image_height =image.length
    val image_width = image.head.length
    val processor = new ij.process.FloatProcessor(image_height,image_width)
    processor.setFloatArray(image)
    new ij.ImagePlus("synthetic image",processor)
  }
  
  def countMaskBlobs(image:Array[Array[Int]]):Int = {
    Blobs.analyzePixelArray(image,"Test").getEntries.head.getValue match{
      case Some(x) => x.toInt
      case _ => 0
    }
  }
  
  
  val signalNoiseGen = for {
    b<-Gen.choose(1,50)
    f<-Gen.choose(b+100,220)
    n<-Gen.choose(15,30)
  } yield (b,f,n)
  
  
  
  def checker (base_count:Int,noisy_image:Array[Array[Float]],derivative_threshold:Double):Boolean = {
    
    val noisy_pixels = noisy_image.flatten
    val max_coeff = noisy_pixels.max
    val pixel_mean = Stats.mean(noisy_pixels)
    val threshold = ObjectThresholding.findThreshold(noisy_pixels.toList,noisy_pixels.min+100,List(),derivative_threshold)
    val thresholded_image = noisy_image.map{row=>row.map{pixel=>{
      if (pixel < threshold) 0
      else 255
    }}}
    ImageIO.drawPixels(ImageIO.makeImage(thresholded_image))
    val test_blob_count = countMaskBlobs(thresholded_image)
    
    val error_percent = (base_count - test_blob_count).toFloat / base_count
    println("error percentage:" + error_percent * 100 + "%")
    if (error_percent > 0.95) checker(base_count,noisy_image,derivative_threshold*(1+error_percent))
    else if (error_percent > 0.5) checker(base_count,noisy_image,derivative_threshold*(1+error_percent)/10)
    else if (error_percent > 0.05) checker(base_count,noisy_image,derivative_threshold*(1+error_percent)/100)
    else if (error_percent < -0.95) checker(base_count,noisy_image,derivative_threshold*(1+error_percent))
    else if (error_percent < -0.5) checker(base_count,noisy_image,derivative_threshold*(1+error_percent)/10)
    else if (error_percent < -0.05) checker(base_count,noisy_image,derivative_threshold*(1+error_percent)/100)
    
    else{
      println(derivative_threshold,Stats.mean(noisy_pixels),Stats.standardDeviation(noisy_pixels),noisy_pixels.max,noisy_pixels.min,Stats.skewness(noisy_pixels),Stats.kurtosis(noisy_pixels))
      scala.math.abs(error_percent) < 0.05
    }
    
  }
  
  def thresholdExplorer(b:Int,f:Int,n:Int):Boolean = {
//    println("b:"+b,"f:"+f,"n"+n)
    val test_base = createPixels(500,500)
    val base_count = countMaskBlobs(test_base.map(r=>r.map(p=>255*p)))
    val noisy_image = noisePixels(test_base,f,b,n)
    val noisy_imageplus = makeImage(noisy_image)
    val noisy_pixels = noisy_image.flatten
    val pixel_mean = Stats.mean(noisy_pixels)
    val pixel_sd = Stats.standardDeviation(noisy_pixels)
    val pixel_range = scala.math.log(noisy_pixels.max - noisy_pixels.min)
    val pixel_kurtosis = scala.math.log(Stats.kurtosis(noisy_pixels))
    val pixel_skewness = scala.math.log(Stats.skewness(noisy_pixels))
    val predictor = (pixel_kurtosis + pixel_skewness + pixel_range) / (pixel_mean + pixel_sd +noisy_pixels.max)

    //    ImageIO.drawPixels(ImageIO.makeImage(test_base))
    ImageIO.drawPixels(noisy_imageplus)
    val derivative_threshold = 0.0000003
    val predicted_dt = 0.044975433935057402 * predictor + -0.0013392212648638082
    println(predicted_dt)
    checker(base_count,noisy_image,predicted_dt)
    
    
  }
  
  val propConsistentCounts = forAllNoShrink(signalNoiseGen){
    case (b,f,n) => thresholdExplorer(b,f,n)
  }
  propConsistentCounts.check

}  
  

  




