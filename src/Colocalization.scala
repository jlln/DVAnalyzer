

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
  
  
  
  
  
}