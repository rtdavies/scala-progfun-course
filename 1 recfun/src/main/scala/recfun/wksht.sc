package recfun

import scala.collection.mutable.ListBuffer

object wksht {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val range = 1 to 2-1                            //> range  : scala.collection.immutable.Range.Inclusive = Range(1)
  val lb = new ListBuffer[Int]                    //> lb  : scala.collection.mutable.ListBuffer[Int] = ListBuffer()
  lb.append(1)
  println(lb)                                     //> ListBuffer(1)
  lb.last                                         //> res0: Int = 1
  println(lb)                                     //> ListBuffer(1)
  val a = new Array[Int](2)                       //> a  : Array[Int] = Array(0, 0)
  a(0) = 1
  a                                               //> res1: Array[Int] = Array(1, 0)
  val range2 = 1 to 0                             //> range2  : scala.collection.immutable.Range.Inclusive = Range()
  range2.length                                   //> res2: Int = 0
}