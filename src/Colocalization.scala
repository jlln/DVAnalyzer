

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
  
  def significanceTesting(r:Array[Array[Float]],g:Array[Array[Float]],b:Array[Array[Float]],nuclear_mask:List[List[Float]]):Result = {
    val x_offset_indices:List[(Int,Int)] = nuclear_mask.map{
      row => {
        val start = row.indexWhere( _ > 0)
        val end = row.slice(start,row.length).indexWhere( _ == 0)
        (start,end)
      }
    }
    val y_offset_indices:List[(Int,Int)] = nuclear_mask.transpose.map{
      row => {
        val start = row.indexWhere( _ > 0)
        val end = row.slice(start,row.length).indexWhere( _ == 0)
        (start,end)
      }
    }
    
    def shiftRow(offsets:(Int,Int),row:List[Float],d:Int):List[Float] = {
      val core_row = row.slice(offsets._1,offsets._2)
      val shift = core_row.length % d
      val shifted_core_row:List[Float] = core_row.slice(core_row.length-shift,core_row.length) ++ core_row.slice(0,core_row.length-shift)
      val new_row:List[Float] = List.fill(offsets._1)(0f) ++ shifted_core_row ++ List.fill(offsets._2)(0f)
      new_row
    }
    
    def shiftXY(x_offsets:List[(Int,Int)],y_offsets:List[(Int,Int)],image:List[List[Float]],dx:Int,dy:Int):List[List[Float]] = {
      val x_shifted = image.zip(x_offsets).map{
        case (r,o) => shiftRow(o,r,dx)
      }
      val xy_shifted = x_shifted.transpose.zip(y_offsets).map{
        case (r,o) => shiftRow(o,r,dy)
      }.transpose
      xy_shifted
    }
    val rf = r.flatten
    val gf = g.flatten
    val bf = b.flatten
    val rg_pearson = Stats.correlationPearson(rf,gf)
    val rb_pearson = Stats.correlationPearson(rf,bf)
    val bg_pearson = Stats.correlationPearson(bf,gf)
    val rl = r.map(r=>r.toList).toList
    val gl = g.map(g=>g.toList).toList
    val shifted_r_rg_rb:List[(Double,Double)] = (1 until 500).par.map{
      i=>{
        val dx = rand.nextInt(r.head.length)+1
        val dy = rand.nextInt(r.length)+1
        val r_shifted = shiftXY(x_offset_indices,y_offset_indices,rl,dx,dy).flatten
        val rg_pearson_shifted = Stats.correlationPearson(r_shifted,gf)
        val rb_pearson_shifted = Stats.correlationPearson(r_shifted,bf)
        (rg_pearson_shifted,rb_pearson_shifted)
      }
      
    }.toList
    val shifted_g_gb:List[Double] = (1 until 500).par.map{
      i=>{
        val dx = rand.nextInt(r.head.length)+1
        val dy = rand.nextInt(r.length)+1
        val g_shifted = shiftXY(x_offset_indices,y_offset_indices,gl,dx,dy).flatten
        Stats.correlationPearson(g_shifted,bf)
      }
    }.toList
    val rg_below = new ResultEntry("RG Significance",Some(shifted_r_rg_rb.filter(x=>x._1 < rg_pearson).length/499d))
    val rb_below = new ResultEntry("RB Significance",Some(shifted_r_rg_rb.filter(x=>x._2 < rb_pearson).length/499d))
    val gb_below = new ResultEntry("GB Significance",Some(shifted_g_gb.filter(x=> x < bg_pearson).length/499d))
    new Result(rf.length,List(rg_below,rb_below,gb_below))
  }
  
}