

/**
 * @author james
 */

import ij._
object Colocalization {


  def threeWayPearson(red:Array[Float],green:Array[Float],blue:Array[Float]):Result = {
    val red_green = new ResultEntry("RedGreenPearson",(Stats.correlationPearson(red,green)))
    val green_blue = new ResultEntry("GreenBluePearson",(Stats.correlationPearson(green,blue)))
    val red_blue = new ResultEntry("RedBluePearson",(Stats.correlationPearson(red,blue)))
    new Result(red.length,List(red_green,green_blue,red_blue))
  }
  
  def overlapFraction(a:Array[Int],b:Array[Int]):Option[Double] = {
    val a_size = a.sum
    val intersection = a.zip(b).filter{
      case (a,b) => a==b
    }.length
    val overlap_fraction = intersection.toDouble/a_size
    if (overlap_fraction.isNaN) None
    else Some(overlap_fraction)
    
  }
  def threeWayManders(mask_r:Array[Int],mask_g:Array[Int],mask_b:Array[Int]):Result = {
    val gor = new ResultEntry("GreenOvelapRed",overlapFraction(mask_r,mask_g))
    val rog = new ResultEntry("RedOverlapGreen",overlapFraction(mask_g,mask_r))
    val rob = new ResultEntry("RedOverlapBlue",overlapFraction(mask_b,mask_r))
    val bor = new ResultEntry("BlueOverlapRed",overlapFraction(mask_b,mask_r))
    val gob = new ResultEntry("GreenOverlapBlue",overlapFraction(mask_g,mask_b))
    val bog = new ResultEntry("BlueOverlapGreen",overlapFraction(mask_b,mask_g))
    new Result(mask_r.length,List(gor,rog,rob,bor,gob,bog))
  }
  
  
  
}