object parallelMerge extends App {


  def merge(lst : List[Int]) : List[Int] = {
    if((!lst.isEmpty) && (!lst.tail.isEmpty)) {
      val n = lst.length/2                                //finding the middle position of the list
      val (left, right) = lst.splitAt(n)                  //Splits into two list, right and left, at the middle which is n
      sort(sort(left) ::: sort(right))                    //Recursively sorts left and right side. Then merge it together in a sorted manner
    }
    else{
      lst
    }
  }

  def sort(lst : List[Int]) : List[Int] = lst match {
    case Nil => lst
    case head::Nil => lst
    case divider::tail => {
      val (left,right) = tail.partition(x => x < divider)   //divide the list into two at the divider, one with values left than the divider
      // and the other more than the divider.
      sort(left) ::: (divider :: sort(right))            //This is where it becomes faster than normal merge sort as it sorts both the
      // left and right side at the same time so if the left and right side are of equal size then it could be up to 2 times faster
    }
  }

  var decreasingL = List[Int]()
  for(i <- 0 to 1000) {
    decreasingL = i+:decreasingL
  }

  val L1 = List()
  val L2 = List(1)
  val L3 = List(10,1)
  val L4 = List(1,3,5,7,9,19,17,15,13,21,33,345,98,43,2,8,4,20,6)
  val L5 = List(11,11,22,22,44,33,55,22,11,111,33,22,44)
  val L6 = List(-1,-4,-7,-9,-33,-55,-32,-22,-51,-135,4,6,213,24,66,98)

  println(merge(L1))
  println(merge(L2))
  println(merge(L3))
  println(merge(L4))
  println(merge(L5))
  println(merge(L6))
  println(merge(decreasingL))


  def time[T](block: => T): Long = {
    val initial_time = System.nanoTime()
    val final_time = System.nanoTime()
    final_time - initial_time
  }

  println("Time " + time {merge(decreasingL)} + " ns")



}





