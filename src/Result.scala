

/**
 * @author james
 */
class Result(area:Int,entries:List[ResultEntry]) {
  val getEntries = entries
  val getArea = area
  def printResults{
    val labels = entries.map(_.getLabel)
    val values = entries.map(_.getValue)
    val output_strings = labels.zip(values).map{ case (l,v)=>l+": "+v.toString}
    output_strings.foreach(println)
  }
  def makeValueString:List[String] = {
   val data = entries.map(e=>e.getValue).map{
     x=> x match{
       case None => "NA"
       case Some(x) => x.toString
       }
     }
   data
   
  }
  override def hashCode = 31*area + entries.map(e=>e.hashCode).sum
  override def equals(other:Any) = other match {
    case that:Result => this.area == that.getArea && this.entries == that.getEntries
    case _ => false
  }
}


class ResultEntry(label:String,value:Option[Double]){
  def getLabel = label
  def isPresent = value.isDefined
  def getValue = value
  def scaleEntry(scaling_factor:Double) = {
    value match{
      case Some(x) => new ResultEntry(this.label, Some(x/scaling_factor))
      case None => new ResultEntry(this.label,None)
    }
  }
  
  override def hashCode = 31 * (31+value.hashCode) + label.hashCode
  override def equals(other:Any) = other match{
    case that:ResultEntry => this.label == that.getLabel && this.value == that.getValue
    case _ => false
  }
}


object Results{
  def addResultEntries(a:(ResultEntry,Int),b:(ResultEntry,Int)):(ResultEntry,Int)={
    val a_result_entry = a._1
    val b_result_entry = b._1
    val a_area = a._2
    val b_area = b._2
    if (a_result_entry.getLabel != b_result_entry.getLabel) throw new Exception("Results must be of the same observation type")
    (a_result_entry.getValue,b_result_entry.getValue) match {
      case (Some(x),Some(y)) => {
        val scaled_x:Double = x*a_area
        val scaled_y:Double = y*b_area
        val total_area:Int = a_area+b_area
        val new_value:Double = (scaled_x + scaled_y)/total_area
        (new ResultEntry(a_result_entry.getLabel,Some(new_value)),total_area)
      }
      case (Some(x),None) => (new ResultEntry(a_result_entry.getLabel,Some(x)),a_area)
      case (None,Some(y)) => (new ResultEntry(a_result_entry.getLabel,Some(y)),b_area)
      case _ => (new ResultEntry(a_result_entry.getLabel,None),0)
    }
  }
  
  def mergeResults(results:List[Result]):Result = {
    val variables:List[String] = results.flatMap(_.getEntries.map(_.getLabel))
    val distinct_variables = variables.distinct
    
    val all_result_entries_and_areas:List[(ResultEntry,Int)] = results.flatMap{
      r=> (r.getEntries.map(rr=> (rr,r.getArea)))
    }
    val result_entries_by_variable_and_areas:List[List[(ResultEntry,Int)]] = {
      distinct_variables.map{
        dv=> all_result_entries_and_areas.filter(v=>v._1.getLabel==dv)
      }
    }
    val area_scaled_values_and_areas:List[(ResultEntry,Int)] = {
      result_entries_by_variable_and_areas.map{
        v => {
          v.drop(1).foldLeft(v.head)(addResultEntries)
          
          }
        }
      }
    
    val total_area_scaled_values:List[ResultEntry] = {
     area_scaled_values_and_areas.map{
       case (r,a) => r.scaleEntry(a)
       } 
    }
    val test_r = area_scaled_values_and_areas.map(_._1)
    val total_area = results.map{x=>x.getArea}.sum
    val nr = new Result(total_area,test_r)
    nr
    
    }
  def concatenateResults(a:Result,b:Result):Result = {
    val area = a.getArea
    if (area != b.getArea) throw new Exception("Results must have equal areas for concatenation")
    val a_labels:List[String] = a.getEntries.map(e=>e.getLabel)
    val b_labels:List[String] = b.getEntries.map(e=>e.getLabel)
    if (a_labels.toSet.intersect(b_labels.toSet).size > 0) throw new Exception("Only different measurement types can be concatenated")
    val concat_entries:List[ResultEntry] = a.getEntries ++ b.getEntries
    new Result(area,concat_entries)
  }
}
