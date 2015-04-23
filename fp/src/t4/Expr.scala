package t4

/**
 * Created by tripp.hu on 4/14/2015.
 */
trait Expr
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr

object Expr{
  def show(e: Expr): String = e match {
    case Number(x) => x.toString
    case Sum(l, r) => show(l) + " + " + show(r)
    case Prod(l, r) => show(l) + " * " + show(r)
  }
}

object test extends App{
  println(Expr.show(new Prod(new Number(1), new Sum(new Number(2), new Number(3)))))
}
