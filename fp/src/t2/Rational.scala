package t2

/**
 * Created by tripp.hu on 3/31/2015.
 */
class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonezore")

  def this(x: Int) = this(x, 1)

  val numer = x
  val denom = y
  val g = gcd(x, y)
  def + (that : Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)

  def unary_- = new Rational(-numer, denom)

  def - (that: Rational) = this + -that

  def < (that: Rational) = this.numer * that.denom < that.numer * this.denom

  def * (that: Rational) = new Rational(this.numer * that.numer , this.denom * that.denom)

  def / (that: Rational) = this * new Rational(that.denom, that.numer)

  def max(that: Rational) = if(this < that) that else this

  private def gcd(a: Int, b:Int): Int = if (b == 0) a else gcd(b, a % b)
  override def toString = numer/g + "/" + denom/g
}

object rationals extends App{
  println(new Rational(10,5))
}
