package t1

/**
 * Created by tripp.hu on 4/23/2015.
 */
object PascalTriangle extends App{
  def pascal(c: Int, r: Int): Int = {
    if(c > r || c < 0 || r < 0) throw new NoSuchElementException
    else if(c == r || c == 0) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  println(pascal(4, 18))
}
