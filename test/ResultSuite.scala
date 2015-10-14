

/**
 * @author james
 */

import org.scalatest._



class ResultSuite extends FunSuite with Matchers {
  val entry_1 = new ResultEntry("foo",Some(5.0))
  val entry_2 = new ResultEntry("foo",Some(5.0))
  test("ResultEntry Equality"){
    new ResultEntry("foo",Some(55)) shouldEqual new ResultEntry("foo",Some(55))
    new ResultEntry("foo",None) shouldEqual new ResultEntry("foo",None)
  }
  test("ResultEntry Inequality"){
    new ResultEntry("foo",Some(55)) should not equal new ResultEntry("bar",Some(55))
    new ResultEntry("foo",Some(55)) should not equal new ResultEntry("foo",Some(53))
    new ResultEntry("foo",None) should not equal new ResultEntry("bar",None)
    new ResultEntry("foo",Some(55)) should not equal new ResultEntry("foo",None)
  }

  test("addResultEntries - two identical & present measurements - equal areas"){
    Results.addResultEntries((entry_1,5),(entry_2,5))._1 shouldEqual (new ResultEntry("foo",Some(5.0)),10)._1
  }
  test("addResultEntries - two identical & present measurements - unequal areas"){
    Results.addResultEntries((entry_1,4),(entry_2,5))._1 shouldEqual (new ResultEntry("foo",Some(5.0)),9)._1
    Results.addResultEntries((entry_1,1),(entry_2,5))._1 shouldEqual (new ResultEntry("foo",Some(5.0)),6)._1
    Results.addResultEntries((entry_1,3),(entry_2,1))._1 shouldEqual (new ResultEntry("foo",Some(5.0)),4)._1
  }
  val entry_3 = new ResultEntry("foo",Some(3.0))
  val entry_4 = new ResultEntry("foo",Some(7.0))
  val entry_4b = new ResultEntry("foo",Some(9.0))
  test ("addResultEntries - two different & present measurements - equal areas"){
    Results.addResultEntries((entry_1,4),(entry_3,4))._1 shouldEqual (new ResultEntry("foo",Some(4.0)),8)._1
    Results.addResultEntries((entry_1,4),(entry_4,4))._1 shouldEqual (new ResultEntry("foo",Some(6.0)),8)._1
  }
  
  val entry_5 = new ResultEntry("Bar",Some(5.0))
  val entry_6 = new ResultEntry("Bar",Some(9.0))
  test ("addResultEntries - two different & present measurements - unequal areas1"){
    //x = entry_5, y = entry_6
    val scaled_x = 5.0*10.0
    val scaled_y = 9.0*20.0
    val total_area = 20.0+10.0
    val new_value = (scaled_x + scaled_y)/total_area

    Results.addResultEntries((entry_5,10),(entry_6,20))._1.getValue shouldEqual Some(new_value)
  }
  test ("addResultEntries - two different & present measurements - unequal areas2"){
    val exp3 = (3.0*9.0 + 8.0* 7.0)/11
    Results.addResultEntries((entry_4,8),(entry_4b,3))._1 shouldEqual (new ResultEntry("foo",Some(exp3)),11)._1
  }
  test ("addResultEntries - two different & present measurements - unequal areas3"){ 
    
    val exp2 = (3*2.0 + 7 * 8)/10
    Results.addResultEntries((entry_3,2),(entry_4,8))._1 shouldEqual (new ResultEntry("foo",Some(exp2)),10)._1
  }
  
  val entry_7 = new ResultEntry("foo",None)
  test ("addResultEntries - missing value"){
    Results.addResultEntries((entry_1,4),(entry_7,5))._1 shouldEqual (new ResultEntry("foo",Some(5.0)),4)._1
    Results.addResultEntries((entry_7,4),(entry_3,5))._1 shouldEqual (new ResultEntry("foo",Some(3.0)),5)._1
  }
  val results_labels = List("foo","bar","newt","tron")
  val result_values_1:List[Option[Double]] = List(Some(5d),Some(25d),Some(5d),Some(50d))
  val result_values_1b:List[Option[Double]] = List(Some(5d),Some(25d),Some(5d),Some(50d))
  val result_values_2:List[Option[Double]] = List(Some(3d),Some(25d),Some(9d),Some(40d))
  val expected_result_1_plus_2_equal_areas:List[Option[Double]] 
    = List(Some(4d),Some(25d),Some(7d),Some(45d))
  val expected_result_1_plus_two_unequal_two_to_one 
    = List(Some((5*100+3*50)/150d),Some(25d),Some((5*100+9*50)/150d),Some((50*100+40*50)/150d))
  val result_values_3:List[Option[Double]] = List(Some(1d),Some(25d),Some(20d),None)
  val expected_result_1_plus_3_equal_areas 
    = List(Some(3d),Some(25d),Some(12.5),Some(50d))
  val result_values_4:List[Option[Double]] = List(Some(1d),None,Some(20d),None)
  def makeResult(area:Int,labels:List[String],values:List[Option[Double]]):Result = {
    val result_entries = labels.zip(values).map{
      case (l,v) => new ResultEntry(l,v)
    }
    new Result(area,result_entries)
  }
  val result_1 = makeResult(100,results_labels,result_values_1)
  val result_1b = makeResult(100,results_labels,result_values_1)
  val result_2 = makeResult(100,results_labels,result_values_2)
  val result_2b = makeResult(50,results_labels,result_values_2)
  val expected_1_and_2b = makeResult(150,results_labels,expected_result_1_plus_two_unequal_two_to_one)
  test("Result Equality"){
    result_1 shouldEqual result_1b
  }
  test("Result Inequality"){
    result_1 should not equal result_2
  }
  
  val expected_1_plus_two_equal_areas = makeResult(200,results_labels,expected_result_1_plus_2_equal_areas)
  test("mergeResults - two different result sets, with equal areas, no missing observations"){
    Results.mergeResults(List(result_1,result_2)) shouldEqual expected_1_plus_two_equal_areas
  }
 
  test("mergeResults - two different result sets, with unequal areas, no missing observations"){
    Results.mergeResults(List(result_1,result_2b)) shouldEqual expected_1_and_2b
  }
 val result_3 = makeResult(100,results_labels,result_values_3)
 val expected_1_and_3 = makeResult(200,results_labels,expected_result_1_plus_3_equal_areas )
  test("mergeResults - missing values and equal areas"){
   Results.mergeResults(List(result_1,result_3)) shouldEqual expected_1_and_3
 }
 
 val concat_test_a = new Result(10,List(new ResultEntry("foo",Some(10))))
 val concat_test_b = new Result(10,List(new ResultEntry("bar",Some(10))))
 val concat_test_c = new Result(10,List(new ResultEntry("tron",Some(10))))
 val concat_test_d = new Result(9,List(new ResultEntry("newt",Some(10))))
 val concat_test_ab = new Result(10,List(new ResultEntry("foo",Some(10)),new ResultEntry("bar",Some(10))))
 val concat_test_abc = new Result(10,List(new ResultEntry("foo",Some(10)),new ResultEntry("bar",Some(10)),new ResultEntry("tron",Some(10))))
 test("Concatenation of results"){

   Results.concatenateResults(concat_test_a,concat_test_b) shouldEqual concat_test_ab
 }
 test("Concatenation of results - inconsistent sizes"){
   intercept[Exception]{
     Results.concatenateResults(concat_test_a,concat_test_d)
   }
 }
 test("Concatenation of results - duplicate_names"){
   intercept[Exception]{
     Results.concatenateResults(concat_test_a,concat_test_ab)
   }
 }
 
}
  
