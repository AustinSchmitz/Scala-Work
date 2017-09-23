

object hwk7 {
  
    def vectorAdd(xs1: List[Int], xs2: List[Int]):List[Int] = {
      val s = xs1 zip xs2
      s.map { case (x,y) => x+y }
    }
    
    def svProduct(xs: Int, xs2: List[Int]):List[Int] = {
       xs2.map { case (y) => xs*y }
    }
    
    def vmProduct(xs1: List[Int], xs2: List[List[Int]]):List[Int] = {
        val x = xs1 zip xs2
        val w = x map { case (a,b) => svProduct(a,b) }
        w reduce ( (c,d) => vectorAdd(c,d) )
    }
    
    def matrixProduct(xs1: List[List[Int]], xs2:List[List[Int]]):List[List[Int]] = {
      if(xs1 == Nil){
          Nil
      }else{
      val x = vmProduct(xs1.head, xs2)
      x::matrixProduct(xs1.tail, xs2)
      }
    }  
    
    def main(args: Array[String]) = {
      println(vectorAdd(List(1,2,3),List(4,5,6)))
      println(svProduct(2,List(1,2,3)))
      println(vmProduct(List(1,2,3), List(List(1,1),List(2,1),List(3,1))))
      println(matrixProduct(List( List(1,2,3), List(1,1,1) ), List( List(1,1), List(2,1), List(3,1))))
  } 
  
}