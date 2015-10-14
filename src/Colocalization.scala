

/**
 * @author james
 */

import ij._
object Colocalization {


  def threeWayPearson(red:Array[Float],green:Array[Float],blue:Array[Float]):Result = {
    val red_green = new ResultEntry("RedGreenPearson",Some(Stats.correlationPearson(red,green)))
    val green_blue = new ResultEntry("GreenBluePearson",Some(Stats.correlationPearson(green,blue)))
    val red_blue = new ResultEntry("RedBluePearson",Some(Stats.correlationPearson(red,blue)))
    new Result(red.length,List(red_green,green_blue,red_blue))
  }
  
  def overlapFraction(a:Array[Int],b:Array[Int]):Double = {
    val a_size = a.sum
    val intersection = a.zip(b).filter{
      case (a,b) => a==b
    }.length
    intersection.toDouble/a_size
    
    
  }
  def threeWayManders(mask_r:Array[Int],mask_g:Array[Int],mask_b:Array[Int]):Result = {
    val gor = new ResultEntry("GreenOvelapRed",Some(overlapFraction(mask_r,mask_g)))
    val rog = new ResultEntry("RedOverlapGreen",Some(overlapFraction(mask_g,mask_r)))
    val rob =  new ResultEntry("RedOverlapBlue",Some(overlapFraction(mask_b,mask_r)))
    val bor = new ResultEntry("BlueOverlapRed",Some(overlapFraction(mask_b,mask_r)))
    val gob = new ResultEntry("GreennOverlapBlue",Some(overlapFraction(mask_g,mask_b)))
    val bog = new ResultEntry("BlueOverlapRed",Some(overlapFraction(mask_b,mask_g)))
    new Result(mask_r.length,List(gor,rog,rob,bor,gob,bog))
  }
  
  
  
}