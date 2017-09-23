

object hwk10 {
    
    def vectorAdd(xs1: List[Int], xs2: List[Int]):List[Int] = {
      
      for( (x,y) <- xs1 zip xs2) yield (x + y)
      
    }
    
    def svProduct(xs: Int, xs2: List[Int]):List[Int] = {
    
      for( y <- xs2 ) yield (xs*y)
      
    }
    
    def vmProduct(xs1: List[Int], xs2: List[List[Int]]):List[Int] = {
        
        val w = for( (x) <- xs1 zip xs2) yield svProduct(x._1,x._2) 
        w reduce vectorAdd 
        
    }
    
    def matrixProduct(xs1: List[List[Int]], xs2:List[List[Int]]):List[List[Int]] = {
      
      for( x <-xs1) yield vmProduct(x,xs2) 
  }
    def zip(xs: List[Int], ys: List[Int]): List[(Int,Int)] =(xs,ys) match {
      case (Nil,_) => Nil
      case (_,Nil) => Nil
      case (x :: xs, y::ys) => (x,y) :: zip(xs,ys)
    }
    
    def main(args: Array[String]) = {
      println(vectorAdd(List(1,2,3),List(4,5,6)))
      println(svProduct(2,List(1,2,3)))
      println(vmProduct(List(1,2,3), List(List(1,1),List(2,1),List(3,1))))
      println(matrixProduct(List( List(1,2,3), List(1,1,1) ), List( List(1,1), List(2,1), List(3,1))))
      println(zip(List(1,2,3),List(2,4,5)));
    } 
}