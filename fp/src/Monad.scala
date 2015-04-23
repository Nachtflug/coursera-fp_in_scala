import scala.collection.immutable.Stack

/**
 * Created by tripp.hu on 4/21/2015.
 */
trait Monad[T] {
  def unit[A](elem: A*): Monad[A]
  def flatMap[A](expr: T => Monad[A]): Monad[A]
  def map[A](expr: T => A): Monad[A] = flatMap[A] {x => unit(expr(x))}
  def foreach(expr: T => Unit) = map(expr);()
  def filter(expr: T => Boolean) = {

  }
}

trait Stack[T] extends Monad[T] {
  override def unit[A](elem: A*): Monad[A] = Stack(elem.toList)
  override def flatMap[A](expr: T => Monad[A]): Monad[A] = flatMap(expr)
  def flatMap[A](expr: T => Stack[A]): Stack[A] = this match {
    case Empty() => new Empty[A]
    case NStack(t, r) => expr(t) pile (r flatMap expr)
  }




  def push(e: T) = new NStack(e, this)
  def pop: Stack[T] = this match {
    case Empty() => throw new Exception("empty.pop")
    case NStack(_, r) => r
  }
  def peek: T = this match {
    case Empty() => throw new Exception("empty.peek")
    case NStack(t, _) => t
  }
  def isEmpty = this match {
    case Empty() => true
    case _ => false
  }
  def pile(that: Stack[T]): Stack[T] =  //重新构建栈1 this 然后连接栈2 that
    if(that.isEmpty) this
    else if(isEmpty) that
    else new NStack[T](peek, pop pile that)
}
case class Empty[T]() extends Stack[T]
case class NStack[T](top: T, rest: Stack[T]) extends Stack[T]


object Stack {
  def apply[T](ts: T*): Stack[T] = apply(ts.toList)

  def apply[T](ts: List[T]): Stack[T] = ts match {
    case t :: r => apply(r) push t
    case _ => new Empty[T]
  }
}

object Test extends App{
  val a = Stack(1,2,3,4,5)
  assert(a.peek == 1)
  assert((a push 10 peek) == 10)
  val b = Stack(6,7,8,9,10)
  assert((a pile b peek) == 1)
  assert((b pile a peek) == 6)
  val c = Stack(a, b)
}