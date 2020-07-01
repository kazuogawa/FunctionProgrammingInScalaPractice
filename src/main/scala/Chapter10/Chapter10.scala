package Chapter10

object Chapter10 {

  trait Monoid[A] {
    //1つにまとめる2項連想演算
    //任意のx: A, y:A, z: Aに対して、op(op(x,y), z) = op(x, op(y, z))が成り立つ
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

  //exercise 10.2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {

    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    def zero: Option[A] = None
  }

  //exercise 10.3
  //引数、戻り値の方が同じである関数をendo関数(endofunction)と呼ぶ。
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a1.compose(a2)

    //これだめなのかー
    //def zero: A => A = _: A => _
    def zero: A => A = (a: A) => a
  }

  //exercise 10.4
  //part2がわからん。
  //def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop

  //monoidを使って畳み込む総称関数
  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  //exercise 10.5
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero)((b: B, a: A) => m.op(b, f(a)))

  //exercise 10.6
  //こたえみた
  //そもそもシグネチャが自分が想像しているものとことなっていた
  //def foldRight[A, B](z: B)(f: (A, B) => B): B
  //全然わからん。カッコが3つになってるのなんで？→curriedしており、先にzを渡して埋めているfに必要なaを埋めている
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = foldMap(as, endoMonoid[B])(f.curried)(z)

  //a => b => f(b,a)がわからん
  //curry化をしているのはわかったが、なんでこんな形になるのか(かけるのか)がわからん・・・
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = foldMap(as, endoMonoid[B])(a => b => f(b, a))(z)

  //モノイドを利用する場合は、平衡畳み込みができるようになる。並列化も可能らしい
  //op(op(a,b), op(c, d))となるため

  //exercise 10.7
  //両半分を再帰的に処理したあと、モノイドを使って結果を結合すること
  //こたえみた。matchでするのかと思っていたがif分岐だった
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (v.isEmpty) m.zero
    else if (v.length == 1) f(v(0))
    else {
      //intの割り算は整数しか出ないから安心
      val (l, r) = v.splitAt(v.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  //第七章やっていないので、10.8飛ばす

  //exercise 10.9
  //いみがわからなかった・・・
  def ordered(ints: IndexedSeq[Int]): Boolean = {
    val mon: Monoid[Option[(Int, Int, Boolean)]] = new Monoid[Option[(Int, Int, Boolean)]] {
      def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] =
        (o1, o2) match {
          case (Some((x1, y1, p)), Some((x2, y2, q))) =>
            //<=が逆矢印に見えたが、比較しているだけか・・・
            Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
          case (x, None) => x
          case (None, x) => x
        }
      val zero: Option[Nothing] = None
    }
    foldMapV(ints, mon)(i => Some((i, i, true))).forall(_._3)
  }

  def main(args: Array[String]): Unit = {
    val words: List[String] = List("Hic", "Est", "Index")
    //(("" + "Hic") + "Est") + "Index"
    val s = words.foldRight(stringMonoid.zero)(stringMonoid.op)
    println(s)
    //"" + (("Hic" + "Est") + "Index"))
    val t = words.foldLeft(stringMonoid.zero)(stringMonoid.op)
    println(t)

    println(foldMap(List(1.1, 2.2, 3.3), intAddition)(_.toInt))
    println(foldMap(List('a', 'b', 'c'), stringMonoid)(_.toString))
  }
}
