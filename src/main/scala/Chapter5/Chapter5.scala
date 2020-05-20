package Chapter5

object Chapter5 {

  trait Stream[+A] {
    //def headOption: Option[A] = this match {
    //  case Stream.empty => None
    //  //h()を使って強制的に評価
    //  case Cons(h, _) => Some(h())
    //}

    //exercise 5.1
    //答え見た。絶対わからん
    //なんでreverse前提で書かれているの？
    def toList: List[A] = {
      @annotation.tailrec
      def go(s: Stream[A], acc: List[A]): List[A] = s match {
        case Cons(h, t) => go(t(), h() :: acc)
        case _ => acc
      }

      go(this, List()).reverse
    }

    //exercise 5.2
    //答え見た。matchでなんとかするのはわかった。
    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
      case _ => Stream.empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 1 => t().drop(n - 1)
      case Cons(_, t) if n == 1 => t()
      case _ => Stream.empty
    }

    //解答は下記。確かに↑のやつより単純。
    def dropAnswer(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 1 => t().drop(n - 1)
      case _ => this
    }

    //exercise 5.3
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
      case _ => Stream.empty
    }

    def exists(p: A => Boolean): Boolean = this match {
      // ||が非正格。p(h())がtrueを返したら走査はそこで終了する
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      //fが第二パラメータに関して非正格なため、パラメータ評価をしないことを選択した場合走査はそこで終了
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def existsViaFoldRight(p: A => Boolean): Boolean =
    //ここのthisはつけてもつけなくてもいいのか。
      this.foldRight(false)((a, b) => p(a) || b)

    //exercise 5.4
    def forAll(p: A => Boolean): Boolean = this.foldRight(true)((a, b) => p(a) && b)

    //exercise 5.5
    //答え見た。foldRight内でifつかっていいのか。
    def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((a, as) =>
      if (p(a)) Stream.cons(a, as) else Stream.empty[A])

    //exercise 5.6
    //def headOptionViaFoldRight: Option[A] =
    //そっかEmptyだったらNone返すからifいらねえな
    //foldRight[Option[A]](None)((h, _) => if (h == Stream.empty) None else Some(h))
    //答えはこれ
    def headOptionViaFoldRight: Option[A] =
      foldRight(None: Option[A])((h, _) => Some(h))


    //exercise 5.7
    def mapViaFoldRight[B](f: A => B): Stream[B] = foldRight[Stream[B]](Stream.empty)((a, as) => Stream.cons(f(a), as))

    def filterViaFoldRight(f: A => Boolean): Stream[A] =
      foldRight[Stream[A]](Stream.empty)((a, as) => if (f(a)) Stream.cons(a, as) else as)

    //そもそも型が違った
    //def appendViaFoldRight(ap: => A): Stream[A] = foldRight(Stream.cons(ap, Stream.empty))((a, as) => Stream.cons(a, as))
    def appendViaFoldRight[B >: A](ap: => Stream[B]): Stream[B] =
      foldRight[Stream[B]](ap)((a, as) => Stream.cons(a, as))

    //答え見た。appendつかうのかー。頭になかった
    def flatMapViaFoldRight[B](f: A => Stream[B]): Stream[B] =
      foldRight[Stream[B]](Stream.empty)((a, as) => f(a).appendViaFoldRight(as))

    //見つけたら即終了なので便利。
    def find(p: A => Boolean): Option[A] = filterViaFoldRight(p).headOptionViaFoldRight

    //exercise 5.8
    def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

    //exercise 5.9
    def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

    //exercise 5.11
    //余再帰。再帰関数はデータを消費するのに対し、余再帰はデータを生成する.
    //unfoldでfを実行するだけでStreamの次の要素を生成する
    //def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = Stream.cons(z, unfold(f(z), f))
    //答え下記...Noneのことを考えていなかった
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z) match {
        case Some((h, s)) => Stream.cons(h, unfold(s)(f))
        case None => Stream.empty
      }

    //exercise 5.12
    //答え見た。0,1のtupleを受け取るのかー。
    val fibsViaUnfold: Stream[Int] = unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }

    //なんか雰囲気で書いたがあってた。うーん。。。。
    def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

    def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(a => Some(a, a))

    val ones: Stream[Int] = unfold(1)(_ => Some(1, 1))

    //exercise5.13
    //こたえみた。this入れるの思いつかなかった。unfold(this)した後に{}ってなに？
    def mapViaUnfold[A, B](f: A => B): Stream[B] = unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }

    //？？？？わからなすぎる
    def takeViaUnfold[A](n: Int): Stream[A] = unfold((this, n)) {
      case (Cons(h, t), 1) => Some(h(), (Stream.empty, 0))
      case (Cons(h, t), n) if n > 1 => Some(h(), (t(), n - 1))
      case _ => None
    }

    //雰囲気で書いたらできた。しくみはわからん
    def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

    //型わからないので答え見た
    def zipWithViaUnfold[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

    //何もわからない。zipWithAllってどこから出てきた・・・
    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
      zipWithAll(s2)((_, _))

    def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
      unfold((this, s2)) {
        case (Empty, Empty) => None
        case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), Stream.empty[B]))
        case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (Stream.empty[A] -> t()))
        case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
      }

    //exercise 5.14
    //思いつきもしない
    def startWith[A](s: Stream[A]): Boolean = {
      zipAll(s).takeWhileViaUnfold(_._2.isDefined) forAll {
        case (h, h2) => h == h2
      }
    }

    //exercise 5.15
    //emptyをappendしわすれた。tailを評価するよりdropしたほうがいいのか？streamを評価しなくて済むからかな
    //def tailsViaUnfold: Stream[Stream[A]] = unfold(this){
    //  case Cons(_, t) => Some((this, t()))
    //  case _ => None
    //}
    def tails: Stream[Stream[A]] =
      unfold(this) {
        case Empty => None
        case s => Some((s, s drop 1))
      } appendViaFoldRight Stream(Stream.empty)

    //exercise5.16
    //型わからなかったので答え見た
    //答え見てもわからない
    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
      foldRight((z, Stream(z)))((a, p0) => {
        // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
        lazy val p1 = p0
        val b2 = f(a, p1._1)
        (b2, Stream.cons(b2, p1._2))
      })._2
  }

  case object Empty extends Stream[Nothing]

  //空でないheadとtailはどちらも非正格。thunk
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    //スマートコンストラクタ
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      //評価の繰り返しを避けるためにheadとtailの評価の遅延値をキャッシュ
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    //これもスマートコンストラクタらしい
    def empty[A]: Stream[A] = Empty

    //ふくすうの要素からStreamを作成するための、可変長の引数を持つメソッド
    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))

    //無限ストリーム
    val ones: Stream[Int] = Stream.cons(1, ones)

    //exercise 5.10
    //答え見た。valで作成していいのか
    val fibs: Stream[Int] = {
      def go(f0: Int, f1: Int): Stream[Int] = cons(f0, go(f1, f0 + 1))

      go(0, 1)
    }

  }

  def main(args: Array[String]): Unit = {
    (1 to 4).map(_ + 10).filter(_ % 2 == 0).map(_ * 3)

    // 非正格性...引数の一つ以上を評価しないという選択が可能
    // 正格関数...引数が常に評価される。ほとんどのプログラミング言語の標準。明記しない限り、Scalaの関数定義は全て正格。
    def square(x: Double): Double = x * x

    // 下記の呼び出しでは、計算済みの42.0をsquareに渡している
    println(square(41 + 1.0))
    // 下記呼び出してでは、squareの本体に入る前にerror式が評価され、squareが何かをする間もなく例外が発生する
    //println(square(sys.error("failure")))

    //&& と ||は非正格
    //これは何も出力されない
    false && {
      println("!!");
      true
    }
    //これも何も出力されない
    true || {
      println("!!");
      false
    }

    // () => Aは0この引数を受け取り、Aを返す関数
    // このような評価されない形式の式をthunk(サンク)と呼ぶ
    def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A =
      if (cond) onTrue() else onFalse()

    val a = 20
    if2(a < 22, () => println("a"), () => println("b"))

    //　これ動かん
    //if2(false, sys.error("fail"), 3)

    // iが2会参照されている
    def maybeTwice(b: Boolean, i: => Int) = if (b) i + i else 0

    val x = maybeTwice(true, {
      println("hi");
      1 + 41
    })
    println(x)


    println("yのしょり")

    //lazy使って明示的にキャッシュすると、iは1度だけ評価される
    def maybeTwice2(b: Boolean, i: => Int) = {
      lazy val j = i
      if (b) j + j else 0
    }

    val y = maybeTwice2(true, {
      println("hi");
      1 + 41
    })

    println(y)


    //object StreamSample{
    //  val ones: Stream[Int] = Stream.cons(1, ones)

    //  def main(): Unit = {
    //    println(Stream.ones.take(5).toList)
    //  }
    //}
    //StreamSample.main()
    println(Stream.ones.take(5).toList)
    println(Stream.ones.exists(_ % 2 != 0))
    //下記はスタックオーバーフローになる
    //println(ones.forAll(_ == 1))
  }
}
