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
  //昇順で並んでいるかどうかを確認するだけか。
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

  sealed trait WC

  //完全な単語を検出していない状態(よくわからない
  case class Stub(chars: String) extends WC

  //words...それまでに検出された単語の数
  //lStub...単語の左側で検出された部分的な単語が格納される
  //rStub...右側で検出された部分的な単語が格納される
  //"lorem ipsum do"だと、Part("lorem", 1, "do")になるらしい
  //"lor sit amet, "だとPart("lor", 2, "")になる.lorで始まって単語が2個あって、部分的なものがないので、rStubは""になる
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  //exercise 10.10
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    //答え見た。自動販売機のやつみたいにmatchでいろいろ作ればよかったのか。fordLeftとかで畳み込む処理を描くのかと思っていた。
    //opだから最小限の処理だけ書けばよかったのか。忘れてた
    def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(c), Stub(d)) => Stub(c + d)
      case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
      case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
      //TODO:isEmpty になることってあるのか？どういう時？
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
    }

    def zero: WC = Stub("")
  }

  //exercise 10.11
  //こたえみた。全然思いつかない
  //M.op(f(x), f(y)) == f(N.op(x,y))が当てはまるらしい
  def wordCount(s: String): Int = {
    def wc(c: Char): WC = if (c.isWhitespace) Part("", 0, "") else Stub(c.toString)

    def unstab(s: String): Int = s.length min 1

    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(s) => unstab(s)
      case Part(l, w, r) => unstab(l) + w + unstab(r)
    }
  }

  trait Foldable[F[_]] {
    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B

    def concatenate[A](as: F[A])(m: Monoid[A]): A

    //exercise 10.15
    //わからん
    def toList[A](fa: F[A]): List[A] = foldRight(fa)(List[A]())(_ :: _)

  }

  //exercise 10.12
  //こたえみた。objectなのか
  object ListFoldable extends Foldable[List] {
    //これ通常のListと何が違うの？
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    //これ通常のListと何が違うの？
    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))

    override def concatenate[A](as: List[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
  }

  object IndexedFoldable extends Foldable[IndexedSeq] {
    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    //なんでここはfoldMapV?
    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
      foldMapV(as, mb)(f)

    override def concatenate[A](as: IndexedSeq[A])(m: Monoid[A]): A =
      foldLeft(as)(m.zero)(m.op)
  }

  object StreamFoldable extends Foldable[Stream] {
    override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    //foldMapとfoldRightの使い分けが謎
    override def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B =
      foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

    override def concatenate[A](as: Stream[A])(m: Monoid[A]): A =
      foldLeft(as)(m.zero)(m.op)
  }

  //exercise 10.14
  //こたえみた。ただのしゃきょうになりつつある
  object OptionFoldable extends Foldable[Option] {
    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
      case None => z
      case Some(a) => f(a, z)
    }

    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
      case None => z
      case Some(a) => f(z, a)
    }

    override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
      case None => mb.zero
      case Some(a) => f(a)
    }

    override def concatenate[A](as: Option[A])(m: Monoid[A]): A =
      foldLeft(as)(m.zero)(m.op)
  }

  //わからん

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
    println(ordered(IndexedSeq(1, 10, 11, 3, 409, 23)))
    println(ordered(IndexedSeq(1, 10, 11)))
  }
}
