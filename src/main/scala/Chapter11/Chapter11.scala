package Chapter11

import scala.math.Ordering.OptionOrdering

object Chapter11 {

  //mapを実装するデータ型
  //Foldableの場合と同様に型コンストラクタF[_]でmapをパラメータ化している
  //(Listは型ではなく、型コンストラクタである。List型の値は存在しないが、それをIntに適用することでList[Int]を生成できる
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]

    //List[(A, B)]を分配したい場合は下記のようなコードになる。unzipとも呼ばれる
    //Listだけでなく、あらゆるファンクタ(ここでいうFのこと)に対応する総称展開関数を記述
    //積を生成する演算らしい・・・
    def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))

    def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
      case Left(fa) => map(fa)(Left(_))
      case Right(fb) => map(fb)(Right(_))
    }
  }

  //distributeとcodistributeのFはファンクタであること以外、Fについては何もわからないため、
  //この法則により、戻り値が引数と同じ形状を持つという確証が得られる

  val listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  //最小のプリミティブとしてunitとflatMapを選択することとし、これらの関数が定義されているデータ型をMonadとよぶことにする。
  //これらunit,flatMap,map,map2があるやつをMonad?
  trait Monad[F[_]] extends Functor[F] {
    //Fで包んであげるやーつ
    def unit[A](a: => A): F[A]

    def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

    def map[A, B](ma: F[A])(f: A => B): F[B] =
      flatMap(ma)(a => unit(f(a)))

    def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
      flatMap(ma)(a => map(mb)(b => f(a, b)))

    //exercise 11.3
    //Chapter4を参考にした
    def sequence[A](lma: List[F[A]]): F[List[A]] =
      lma.foldRight[F[List[A]]](unit(List[A]()))((x, y) => map2(x, y)(_ :: _))

    def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
      la.foldRight[F[List[B]]](unit(List[B]()))((x, y) => map2(f(x), y)(_ :: _))

    //exercise 11.4
    //答え見た。何を言っているのかわからない。答え見たら意味がわかった
    def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

    //exercise 11.6
    //traverse当てて、List(A, Boolean)にしたあと、filterかける？
    //わからんので答えみる
    //何しているのか理解不能。勉強会の時に聞く
    def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
      ms.foldRight(unit(List[A]()))((x, y) =>
        compose(f, (b: Boolean) => if (b) map2(unit(x), y)(_ :: _) else y)(x))

    //exercise 11.7
    //11.6で書いてあるのを見てしまった
    //モナドのクライスリ射の合成関数らしい
    //モノイドの結合律みたく、下記を満たすらしい
    //compose(compose(f,g),h) == compose(f, compose(g, h))
    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
      a => flatMap(f(a))(g)

    //exercise 11.8
    //composeがflatMap使っているのにflatmapを実装って意味わからん。なんで？
    //答え見た
    def flatMapViaCompose[A, B](ma: F[A])(f: A => F[B]): F[B] =
      compose((_: Unit) => ma, f)(())

    //exercise 11.12
    //flatMapを使って実装
    //こたえみた
    //flatMap = flatten + mapなのでflattenするだけってことか
    def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  }

  //ふぁんくたはmapがある。

  //exercise 11.1
  //ParとParsersは答えからとってきた

  import Par._

  val parMonad: Monad[Par] = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }

  //そもそも型ちげえし、defだしわからんことだらけ
  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new Monad[P] {
    //succeedってなに？
    def unit[A](a: => A): P[A] = p.succeed(a)

    override def flatMap[A, B](ma: P[A])(f: A => P[B]): P[B] = p.flatMap(ma)(f)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    //普通に間違えたw何か入っていれば必ずSomeなので確かにSomeでいいよね。
    //override def unit[A](a: => A): Option[A] = Option(a)
    override def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
  }
  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)

    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma.flatMap(f)
  }
  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
  }

  //exercise 11.2
  //答え見た。S,Aを1つの型にして、それをMonadにする必要があったのか...すごい…

  import Chapter6.Chapter6.State

  class StateMonads[S] {
    type StateS[A] = State[S, A]

    // We can then declare the monad for the `StateS` type constructor:
    val monad: Monad[StateS] = new Monad[StateS] {
      def unit[A](a: => A): State[S, A] = State(s => (a, s))

      override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
        st flatMap f
    }
  }

  //exercise 11.3,4, 6,7,8はMonadの中
  //9はわからん


  def main(args: Array[String]): Unit = {
    //exercise 11.5
    //ListモナドでreplicateMを使うとnで指定された分のList[List[A]]ができると思っていたが違った
    //Listの要素の全組み合わせができた
    println(listMonad.replicateM(3, List(1, 2, 3)))
    //OptionMonadは想像通り、nで指定した分の要素を返す
    println(optionMonad.replicateM(3, Option(1)))
    //replicateMはnで指定した分、Fの全組み合わせをListにして返す

    //結合律の証明
    //None.flatMap(f).flatMap(g) == None.flatMap(a => f(a).flatMap(g))
    //None.flatMap(f)は全てのfに対してNoneなため None == Noneになる
    //xがNoneの場合はこの法則が適用される。って日本語おかしくないか？
    //xがSome(v)でvが任意の場合は下記を満たすため、NoneでもSomeでも法則が適用される
    //x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))
    //Some(v).flatMap(f).flatMap(g) == Some(v).flatMap(a => f(a).flatMap(g)) xをSome(v)に置き換え
    //f(v).flatMap(g) == (a => f(a).flatMap(g))(v) Some(v).flatMap(...)の定義を適用
    //f(v).flatMap(g) == f(v).flatMap(g) 関数適用を簡約

    //ゼロがappendの単位元
    //composeの単位元はunit。よって
    //compose(f, unit) == f
    //compose(unit, f) == f
    //flatMapでも表現できるが明白ではない
    //flatMap(x)(unit) == x
    //flatMap(unit(y))(f) == f(y)

    //exercise 11.10, 11
    //証明は諦め・・・


  }
}
