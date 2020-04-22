package Chapter5

object Chapter5 {
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

    trait Stream[+A] {
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

      //ふくすの要素からStreamを作成するための、可変長の引数を持つメソッド
      def apply[A](as: A*): Stream[A] =
        if (as.isEmpty) empty
        else cons(as.head, apply(as.tail: _*))
    }
    //def headOption: Option[A] = this match {
    //  case Empty => None
    //h()を使って強制的に評価
    //  case Cons(h, t) => Some(h()) 1
    //}


  }
}
