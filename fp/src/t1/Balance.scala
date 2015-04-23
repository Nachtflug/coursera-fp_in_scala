package t1

import scala.annotation.tailrec

/**
 * Created by tripp.hu on 3/27/2015.
 */
object Balance extends App{
  def balance(chars: List[Char]) :Boolean = {
    @tailrec
    def loop(chars: List[Char], count: Int): Boolean = {
      if(chars.isEmpty) count == 0
      else if(chars.head == '(') loop(chars.tail, count + 1)
      else if(chars.head == ')')
        if(count - 1 < 0) false
        else loop(chars.tail, count - 1)
      else loop(chars.tail, count)
    }
    loop(chars, 0)
  }

  println(balance("())(".toList))
}
