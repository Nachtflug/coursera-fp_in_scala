package t2

/**
 * Created by tripp.hu on 4/2/2015.
 */
object FunSets extends App{
  //集合：给定整数，这个函数会告诉你是否包含
  type Set = Int => Boolean

  //同上
  def contains(s: Set, elem: Int): Boolean = s(elem)

  //返回一个单元素的集合
  def singletonSet(elem: Int): Set = _ == elem // x => x == elem 的简便写法

  //返回俩集合并集
  def union(s: Set, t: Set): Set = x => s(x) || t(x)

  //返回俩集合并集交集
  def intersect(s: Set, t:Set): Set = x => s(x) && t(x)

  //返回 s 与 t补集 的交集
  def diff(s: Set, t: Set): Set = intersect(s, !t(_)) // x => !t(x)的简便写法

  //返回s经过p过滤后的集合
  //这里的p在代码内部完全可以当成Set来看的样子
  def filter(s: Set, p: Int => Boolean): Set = intersect(s, p)

  //集合中所有元素是否都满足 条件p
  def forall(s: Set, p: Int => Boolean): Boolean = {
    //需要制定一个边界，此处为{-1000,1000}
    val x = 1000
    def iter(a: Int): Boolean = {
      if(a > x) true
      else if(diff(s, p)(a)) false
      else iter(a + 1)
    }
    iter(-x)
  }

  //集合中所有元素是否有一个或以上元素满足 条件p
  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, !p(_))

  //将集合中所有元素作f变换后 得到新集合
  def map(s: Set, f: Int => Int): Set = x => exists(s, f(_) == x)

  private def printSingle(v: Int) = {printf(v + " ", null); true}

  //打印集合中所有元素
  def print(s: Set) = {forall(s, printSingle); println()}

  //测试
  val a = singletonSet(1)
  val b = singletonSet(2)
  val c = union(a, b)
  print(c)
  print(diff(c, a))
  print(intersect(c, a))
  print(filter(c , _ > 1))
  println(forall(c, _ > -1))
  println(exists(c, _ < -1))
  print(map(c, _ * 2))
}
