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
  
  
  
  def isPixelAvailable(x:Int,y:Int,image:Array[Array[Int]],object_number:Int):Boolean = {
    val x_coords = x match {
      case 0 => List(0,1)
      case ix if ix == image.head.length-1 => List(ix,ix-1)
      case xx => List(xx,xx-1,xx+1)
    }
    val y_coords = y match {
      case 0 => List(0,1)
      case iy if iy == image.head.length-1 => List(iy,iy-1)
      case yy => List(yy,yy-1,yy+1)
    }
    val neighbour_coords = for (ix<-x_coords;iy<-y_coords) yield (ix,iy)
    val neighbour_values = neighbour_coords.map{
      case (x,y) => image(y)(x)
    }.toSet
    ((image(y)(x)== 0 && neighbour_values == Set(0,object_number) )||( image(y)(x)== 0 && neighbour_values == Set(object_number)))
    
  }
  
  def getEmptyNeighbours(x:Int,y:Int,image:Array[Array[Int]],object_number:Int):List[(Int,Int)] = {
    val x_coords = x match {
      case 0 => List(0,1)
      case ix if ix == image.head.length-1 => List(ix,ix-1)
      case xx => List(xx,xx-1,xx+1)
    }
    val y_coords = y match {
      case 0 => List(0,1)
      case iy if iy == image.head.length-1 => List(iy,iy-1)
      case yy => List(yy,yy-1,yy+1)
    }
    val coords = for (ix<-x_coords;iy<-y_coords) yield (ix,iy)
    coords.filter{
      case (x,y) => isPixelAvailable(x,y,image,object_number)
    }
  }
  
  def expandBlob(image:Array[Array[Int]],object_number:Int,target_size:Int):Array[Array[Int]] = {
    val coordinates = for (x<-0 until image.head.length ; y<- 0 until image.length) yield (x,y)
    val blob_coordinates = coordinates.filter{
      case (x,y) => image(y)(x) == object_number
    }

    if (blob_coordinates.length == target_size) image
    else {
      val potential_expansions = blob_coordinates.map{
        case (x,y) => getEmptyNeighbours(x,y,image,object_number)
      }.flatten
      if (potential_expansions.length == 0) image
      else {
        val new_pixel = potential_expansions(rand.nextInt(potential_expansions.length))
        val new_image = image.updated(new_pixel._2,image(new_pixel._2).updated(new_pixel._1,object_number))
        expandBlob(new_image,object_number,target_size)
      }
    }
    
  }
  
  def createBlob(image:Array[Array[Int]],max_size:Int,object_number:Int):Array[Array[Int]] = {
    val x = rand.nextInt(image.head.length)
    val y = rand.nextInt(image.length)
    if (isPixelAvailable(x,y,image,object_number)){
      val new_image = image.updated(y,image(y).updated(x,object_number))
      val updated_image:Array[Array[Int]] = expandBlob(new_image,object_number,max_size)
      updated_image
    }
    else{
      createBlob(image,max_size,object_number)
    }
  }
  
  
  def proceduralBlobs(x_size:Int,y_size:Int,n_blobs:Int):Array[Array[Int]] = {
    var image = Array.fill(y_size){Array.fill(x_size){0}}
    val max_size = (x_size * y_size)/ (n_blobs * n_blobs)
    (0 until n_blobs).map{
      n=>{
        image = createBlob(image,max_size,n)
      }
    }
    image
  }
  
  
  def createPixels(x:Int,y:Int,bc:Double,ep:Double):Array[Array[Int]] = {
    val image = Array.fill(y){Array.fill(x){0}}
    def pixelIter(ix:Int,iy:Int,image:Array[Array[Int]],bc:Double,ep:Double):Array[Array[Int]] = {
      val neighbours = (ix,iy) match {
        case (0,0) => List()
        case (0,iiy) => List(image(iiy-1)(0))
        case (iix,0) => List(image(0)(iix-1))
        case (iix,iiy) if iix < x-1 && iiy < y-1 => List(image(iiy)(iix-1),image(iiy-1)(ix),image(iiy-1)(ix+1))
        case (_,_) => List()
      }
      val cutoff = bc+neighbours.sum * ep
      val pixel = if (rand.nextFloat < cutoff) 1 else 0
      image(iy).update(ix,pixel)
      image
    }
    (0 until x).map{
      xx=>{
        (0 until y).map{
          yy=> pixelIter(xx,yy,image,bc,ep)
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