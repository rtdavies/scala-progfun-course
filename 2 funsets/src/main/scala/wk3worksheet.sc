package funsets

object wk3worksheet {
  println("hi")                                   //> hi
  val x = new Rational(1, 3)                      //> x  : funsets.Rational = 1/3
  val y = new Rational(5, 7)                      //> y  : funsets.Rational = 5/7
  val z = new Rational(3, 2)                      //> z  : funsets.Rational = 3/2
  x.add(y)                                        //> res0: funsets.Rational = 22/21
  x.neg                                           //> res1: funsets.Rational = 1/-3
  x.sub(y)                                        //> res2: funsets.Rational = 8/-21
  y.sub(x)                                        //> res3: funsets.Rational = 8/21
  x.sub(y).sub(z)                                 //> res4: funsets.Rational = -79/42
  y.add(y)                                        //> res5: funsets.Rational = 10/7
  type Set = Int => Boolean
  val r1 = new Range(3,6,1)                       //> r1  : scala.collection.immutable.Range = Range(3, 4, 5)
}



class Rational(n: Int, d: Int) {
  private def gcd(a: Int, b: Int): Int = if (b==0) a else gcd(b, a%b)
  private val g = gcd(n, d)
  
  val numer: Int = n/g
  val denom: Int = d/g
  
  def add(that: Rational) = {
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)
  }
  
  def sub(that: Rational) = add(that.neg)
  
  def neg() = {
    new Rational(-numer, denom)
  }
  
  override def toString() = {
    numer + "/" + denom
  }
}