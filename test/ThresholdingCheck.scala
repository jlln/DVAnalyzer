

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
      val cutoff = 0.007+neighbours.sum * 0.45
      println(cutoff)
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
  
  
  val t1 = createPixels(500,500)
  val t1c = countMaskBlobs(t1.map(r=>r.map(p=>255*p)))
  println(t1c)
  val t1cp = noisePixels(t1,200,50,40)
  val ti1 = makeImage(t1cp)
  ImageIO.drawPixels(ti1)
  val t1cpp = t1cp.flatten
  val til_mean = Stats.mean(t1cpp)
  val threshold = 0.000035
  val image_thresholded = ObjectThresholding.thresholdTestFunction(threshold, t1cp)
  ImageIO.drawPixels(ImageIO.makeImage(image_thresholded))
  val ni_bc = countMaskBlobs(image_thresholded)
  println(t1c)
  println(ni_bc)
  

}  
  

  




