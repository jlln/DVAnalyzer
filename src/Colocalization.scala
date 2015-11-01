

/**
 * @author james
 */

import ij._
object Colocalization {
  val rand = new scala.util.Random(123456)

  def threeWayPearson(red:Array[Float],green:Array[Float],blue:Array[Float]):Result = {
    val red_green = new ResultEntry("RedGreenPearson",(Some(Stats.correlationPearson(red,green))))
    val green_blue = new ResultEntry("GreenBluePearson",(Some(Stats.correlationPearson(green,blue))))
    val red_blue = new ResultEntry("RedBluePearson",(Some(Stats.correlationPearson(red,blue))))
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
    
    def offsets1D(pixels:List[List[Int]]):List[(Int,Int)] = {
      //offsets are the number of zeros preceding and following the non-zero pixel block
       pixels.map{
        row =>{
          val left_side = row.indexWhere(_ > 0)
          if (left_side == -1) (0,0)
          else{
            val remainder = row.slice(left_side,row.length)
            val right_side = remainder.reverse.indexWhere(_ > 0)
            if (right_side == -1) (left_side,0)
            else (left_side,right_side)
          }
        }
      }
    }
    
   def findOffsets(pixels:List[List[Int]]):(List[(Int,Int)],List[(Int,Int)]) = {
      val x_offsets:List[(Int,Int)] = offsets1D(pixels)
      val y_offsets:List[(Int,Int)] = offsets1D(pixels.transpose)
      (x_offsets,y_offsets)
   }
   
    
   
   def shiftRow(row:List[Float],offsets:(Int,Int),d:Int):List[Float] = {
     val core_row = row.slice(offsets._1,row.length-offsets._2)
     if (core_row.length == 0) row
     else{
       val delta = if (d <= core_row.length) d else d % core_row.length
       val left_side:List[Float] = List.fill(offsets._1)(0f) ++ core_row.slice(core_row.length-delta,core_row.length)  
       val right_side:List[Float] = core_row.slice(0,core_row.length-delta) ++ List.fill(offsets._2)(0f)
       left_side ++ right_side
     }
   }
   
   def shiftRows(rows:List[List[Float]],offsets:List[(Int,Int)],d:Int):List[List[Float]] = {
     rows.zip(offsets).map{
       case (r,o) =>shiftRow(r,o,d)
     }
   }
   
   def shuffleSlice(slice:List[List[Float]],offsets:(List[(Int,Int)],List[(Int,Int)]),dx:Int,dy:Int):List[List[Float]] = {
     val x_shifted = shiftRows(slice,offsets._1,dx)
     shiftRows(x_shifted.transpose,offsets._2,dy).transpose
   }
   def shuffleImage(image:List[List[List[Float]]],offsets:List[(List[(Int,Int)],List[(Int,Int)])],dx:Int,dy:Int):List[List[List[Float]]] = {
       image.zip(offsets).map{
         
         case (i,o) => shuffleSlice(i,o,dx,dy)
       }
   }
   def constrainedDisplacementTesting(image_r:List[List[List[Float]]],image_g:List[List[List[Float]]],image_b:List[List[List[Float]]],nucleus_mask:List[List[List[Int]]]):Result = {
     val flat_r = image_r.flatten.flatten
     val flat_g = image_g.flatten.flatten
     val flat_b = image_b.flatten.flatten
     val original_pr_value_rg = Stats.correlationPearson(flat_r,flat_g)
     val original_pr_value_rb = Stats.correlationPearson(flat_r,flat_b)
     val original_pr_value_bg = Stats.correlationPearson(flat_b,flat_g)
     val offsets = nucleus_mask.map(r=>findOffsets(r))
     val r_shufflings:List[(Double,Double)] = (1 until 100).par.map{ i=>
        val dx = rand.nextInt(image_r.head.length)+1
        val dy = rand.nextInt(image_r.length)+1
        val shuffled_r:List[Float] = shuffleImage(image_r,offsets,dx,dy).flatten.flatten
        val new_pr_value_rg = Stats.correlationPearson(shuffled_r,flat_g)
        val new_pr_value_rb = Stats.correlationPearson(shuffled_r,flat_b)
        (new_pr_value_rg,new_pr_value_rb)
     }.toList
     val g_shufflings:List[Double] = (1 until 100).par.map{i=>
        val dx = rand.nextInt(image_r.head.length)+1
        val dy = rand.nextInt(image_r.length)+1
        val shuffled_g = shuffleImage(image_g,offsets,dx,dy).flatten.flatten
        val new_pr_value_bg = Stats.correlationPearson(shuffled_g,flat_b)
        new_pr_value_bg
     }.toList
     
     val rg_below = new ResultEntry("RG_coloc_p_value",Some(r_shufflings.filter(x=>x._1 < original_pr_value_rg).length/100d))
     val rb_below = new ResultEntry("RB_coloc_p_value",Some(r_shufflings.filter(x=>x._2 < original_pr_value_rb).length/100d))
     val bg_below = new ResultEntry("BG coloc_p_value",Some(g_shufflings.filter(x=>x < original_pr_value_bg).length/100d))
     val corr_rg = new ResultEntry("RG_pearsons_r",Some(original_pr_value_rg))
     val corr_rb = new ResultEntry("RB_pearsons_r",Some(original_pr_value_rb))
     val corr_bg = new ResultEntry("BG_pearsons_r",Some(original_pr_value_bg))
     new Result(flat_r.length,List(rg_below,rb_below,bg_below,corr_rg,corr_rb,corr_bg))
     }
     

}