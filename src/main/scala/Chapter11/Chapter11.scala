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
  trait Monad[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]

    def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

    def map[A, B](ma: F[A])(f: A => B): F[B] =
      flatMap(ma)(a => unit(f(a)))

    def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
      flatMap(ma)(a => map(mb)(b => f(a, b)))
  }

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
      override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
        st flatMap f
    }
  }

  def main(args: Array[String]): Unit = {

  }
}
