

abstract class Exp {

}


case class Const(x: Int) extends Exp
case class Var(s: String) extends Exp
case class Plus(x: Exp, y: Exp) extends Exp
case class Times(x: Exp, y: Exp) extends Exp
case class Pow(x: Exp, y:Int) extends Exp

object hwk8 extends Exp{
  
 def print(e: Exp): String = e match{
    case Const(y: Int) => y.toString
    case Var(s: String) => s
    case Plus(e1: Exp,e2: Exp) => "( " + print(e1) + " + " + print(e2) + " )"
    case Pow(e1:Exp ,e3: Int) => "( " + print(e1) + " ^ " + e3 + " )"
    case Times(e1: Exp, e2: Exp) => "( " + print(e1) + " * " + print(e2) + " )" 
  }
  
  def deriv(e1: Exp, e2: Exp): Exp =
      (e1,e2) match {
        case (Const(x), _) => Const(0)
        case (Var(e1), Var(e2)) => if (e1==e2) Const(1) else Const(0)
        case (Plus(e1,e2), Var(x)) => Plus(deriv(e1, Var(x)),deriv(e2, Var(x)))
        case (Times(e1,e2), Var(x)) => Plus(Times(deriv(e1, Var(x)), e2),Times(e1, deriv(e2, Var(x))))
        case (Pow(e1,n), Var(x)) => Times(Times(Const(n), Pow(e1, n-1)), deriv(e1, Var(x)))
      }
 
  def simp(e: Exp): Exp = e match {
    case Const(x) => Const(x)
    case Var (x) => Var(x)
    case Times(Const(1),e2) => simp(e2)
    case Times(e1, Const(1)) => simp(e1)
    case Times(Const(0), e2) => Const(0)
    case Times(e1, Const(0)) => Const(0)
    case Times(e1,e2) => Times(e1,e2)
    case Plus(Const(0), e2) => simp(e2)
    case Plus(e1, Const(0)) => simp(e1)
    case Plus(e1,e2) => Plus(e1,e2)
    case Pow(e1, 0) => Const(1)
    case Pow(e1, 1) => e1
    case Pow(e1,n) => Pow(e1,n)
  }
  
  def simplify(e: Exp): Exp = e match{
    case Const(x) => Const(x)
    case Var(x) => Var(x)
    case Times(e1,e2) => simp(Times(simplify(e1),simplify(e2)))
    case Plus(e1,e2) => simp(Plus(simplify(e1), simplify(e2)))
    case Pow(e1,n) => simp(Pow(simplify(e1),n))
  }
  
   
  def main(args: Array[String]){
    val e = Times( Times(Var("x"), Var("y")), Plus(Var("x"),Const(3)))
    val e1 = Pow (Var("x"), 4)
    
    println(print(e))
    println(print(e1))
    
    println(print(deriv(e,(Var("x")))))
    println(print(deriv(e1,(Var("x")))))
    
    println(print(simplify(deriv(e,(Var("x"))))))
    println(print(simplify(deriv(e1,(Var("x"))))))
  }
}
