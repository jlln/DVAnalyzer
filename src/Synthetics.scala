import ij._ 
import ij.plugin.filter.ParticleAnalyzer 

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._


import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

object Synthetics {
  val rand = new scala.util.Random(12345)
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
      val cutoff = 0.008+neighbours.sum * 0.47
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
  
  def getNeighbours(pixel:(Int,Int),image:List[List[Int]]):List[(Int,Int)] = {
    val x = pixel._1
    val y = pixel._2
    val x_shifts = List(x-1,x,x+1).filter(xx=> xx > -1 & xx < image.length)
    val y_shifts = List(y-1,y,y+1).filter(yy=> yy > -1 & yy < image.head.length)
    val new_neighbours = for (xx <- x_shifts ; yy <- y_shifts) yield (xx,yy)
    new_neighbours
  }
  def getNeighbourValues(pixel:(Int,Int),image:List[List[Int]]):List[Int] = {
    val neighbours:List[(Int,Int)] = getNeighbours(pixel,image)
    neighbours.map{
      case (x,y) => image(y)(x)
    }
  }
  
//  def expandObject(image:List[List[Int]],target_size:Int,neighbours:List[(Int,Int)],object_number:Int):List[List[Int]] = {
//    val candidates = neighbours.flatMap(n=>getNeighbours(n,image))
//    val acceptable_candidates = neighbours.filter{n=>{
//      val neighbour_values = getNeighbourValues(n,image).distinct
//      neighbour_values.diff(List(object_number)) == List(0)
//      }
//    }
//    val chosen_candidate = acceptable_candidates(rand.nextInt(acceptable_candidates.length))
//  }
  
 
  
  
  def noisePixels(template:Array[Array[Int]],foreground_mean:Int,background_mean:Int,noise:Int):Array[Array[Double]] = {
    val image_width = template.head.length
    val pixels = template.flatten
    val noised_pixels = pixels.map{
      p => (if (p == 1) foreground_mean else background_mean) + rand.nextGaussian * noise
    }.map(p=>p.toDouble)
    val np = noised_pixels.grouped(image_width).toArray
    Stats.convolveGaussian2D(np)
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
    b<-Gen.choose(1,50)
    f<-Gen.choose(b+110,240)
    n<-Gen.choose(5,20)
  } yield (b,f,n)
  
}