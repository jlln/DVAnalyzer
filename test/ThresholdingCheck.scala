

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

  def createPixels(x:Int,y:Int,image:Array[Array[Int]]):Array[Array[Int]] = {

    if (image.last.length == x){
      if (image.length == y){ //The image is done
        println("image done")
        image
      }
      else { //start drawing a new row
        println("new row")
        println(image.last)
        val new_image = image :+ Array(0)
        createPixels(x,y,new_image) 
      }
    }
    else{ //draw the pixel
      val iy = image.length - 1
      val ix = image.last.length
      val p_threshold = (ix,iy) match {
        
        case (0,0) => 0.9
        case (0,_) => 0.9 - 0.1 * (image(iy-1)(0) + image(iy-1)(1))
        case (_,0) => 0.9 - 0.1 * (image(iy)(ix-1))
        case (iix,_) if iix < x-1 => 0.8 - 0.15 * (image(iy-1)(ix-1) + image(iy-1)(ix)+ image(iy-1)(ix+1))
        case (_,_) => 0.9 - 0.1 * (image(iy-1)(ix-1) + image(iy-1)(ix))
        
      }
      val p = if (p_threshold < rand.nextFloat) 1 else 0
      val new_row = image.last :+ p

      image.update(image.length-1,new_row)
      createPixels(x,y,image)
    }
  }
  
  val t1 = createPixels(50,50,Array(Array(0))).map(x=>x.toArray.map(x=>255*x)).toArray
  
  ImageIO.drawPixels(ImageIO.makeImage(t1))
  
}  
  

  




