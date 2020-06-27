package Chapter10

object Chapter10 {

  trait Monoid[A] {
    //1つにまとめる2項連想演算
    //任意のx: A, y:A, z: Aに対して、op(op(x,y), z) = op(x, op(x, y))が成り立つ
    def op(a1: A, a2: A): A

    //演算の単位元。任意のx: Aに対し op(x, zero) == xとop(zero, x) == xが成り立つ
    def zero: A
  }

  val stringMonoid: Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2

    def zero = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    val zero: List[A] = Nil
  }

  //exercise 10.1
  //加算
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2

    def zero = 0
  }
  //乗算
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2

    def zero = 1
  }
  //Or
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    def zero = false
  }
  //And
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    def zero = true
  }

  def main(args: Array[String]): Unit = {

  }
}
