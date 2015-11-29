

/**
 * @author james
 */

import ij._
object Colocalization {
  val rand = new scala.util.Random(123456)

  def correlationPearsonEntries(channels:List[List[Float]],n:Int):List[ResultEntry] = {
    val current_channel_lab = n+1
    
    
    if (channels.length == 2){
      val next_channel_lab = current_channel_lab+1
      val label = f"Channel$current_channel_lab%sChannel$next_channel_lab%sPearsonCorrelation"
      List(new ResultEntry(f"Channel$current_channel_lab%sChannel$next_channel_lab%sPearson",Some(Stats.correlationPearson(channels.head,channels.last))))
    }
    else{
      val current_channel = channels.head
      val channel_tail = channels.tail
      val current_pairs:List[ResultEntry] = channel_tail.zipWithIndex.map{
        case (c,i) =>{
          val next_channel_lab = current_channel_lab+i+1
          val label = f"Channel$current_channel_lab%sChannel$next_channel_lab%sPearsonCorrelation"
          new ResultEntry(label,Some(Stats.correlationPearson(current_channel,c)))
          
        }
      }
      current_pairs ++ correlationPearsonEntries(channels.tail,n+1)
    }
  
  
  }
  
  def correlationPearson(condition:String,channels:List[List[Float]]):Result = {
    val entries = correlationPearsonEntries(channels,0)
    new Result(condition,channels.head.length,entries)
  }
  
  def overlapFraction(a:Traversable[Int],b:Traversable[Int]):Option[Double] = {
    val a_size = a.sum
    val intersection = a.toList.zip(b.toList).filter{
      case (a,b) => a==b
    }.length
    val overlap_fraction = intersection.toDouble/a_size
    if (overlap_fraction.isNaN) None
    else Some(overlap_fraction) 
  }
  
    
  def mandersEntries(channels:List[List[Int]],n:Int):List[ResultEntry] = {
    val current_channel_lab = n+1
    if (channels.length == 2){
      val next_channel_lab = current_channel_lab+1

      val forward = new ResultEntry(f"Channel$current_channel_lab%sChannel$next_channel_lab%sManders",overlapFraction(channels.head,channels.last))
      val reverse = new ResultEntry(f"Channel$next_channel_lab%sChannel$current_channel_lab%sManders",overlapFraction(channels.head,channels.last))
      List(forward,reverse)
    }
    else{
      val current_channel = channels.head
      val channel_tail = channels.tail
      val current_pairs:List[ResultEntry] = channel_tail.zipWithIndex.map{
        case (c,i) =>{
          val next_channel_lab = current_channel_lab+i+1
          val forward = new ResultEntry(f"Channel$current_channel_lab%sChannel$next_channel_lab%sManders",overlapFraction(channels.head,channels.last))
          val reverse = new ResultEntry(f"Channel$next_channel_lab%sChannel$current_channel_lab%sManders",overlapFraction(channels.head,channels.last))
          List(forward,reverse)
          
        }
      }.flatten
      current_pairs ++ mandersEntries(channels.tail,n+1)
    }
  
  
  }
  
  def manders(condition:String,channels:List[List[Int]]):Result = {
    val area = channels.head.size
    val entries = mandersEntries(channels,0)
    new Result(condition,area,entries)
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
   

   
   

     

}