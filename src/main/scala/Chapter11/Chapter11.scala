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

    //exercise 11.13
    //joinとmapでflatMapとcomposeをつくる
    //型パズル？
    def flatMapViaJoinAndMap[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

    def composeViaJoinAndMap[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
      a => join(map(f(a))(g))

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

  //exercise 11.17
  //Idを使ってMonad[Id]を実装
  case class Id[A](value: A) {
    //そっか自分がvalue持ってるからmaいらないのか
    //def map[A, B](ma: Id[A])(f: A => B): Id[B] = Id(f(ma.value))

    //def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = f(ma.value)

    def map[B](f: A => B): Id[B] = Id(f(value))

    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  }

  val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)

    @scala.annotation.tailrec
    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = flatMap(ma)(f)
  }

  type IntState[A] = State[Int, A]

  object IntStateMonad extends Monad[IntState] {
    override def unit[A](a: => A): IntState[A] = State(s => (a, s))

    override def flatMap[A, B](ma: IntState[A])(f: A => IntState[B]): IntState[B] = ma.flatMap(f)
  }

  //typeを分けて書かずにinlineで書くことも可能らしい
  object IntStateMonadInline extends Monad[({type IntState[A] = State[Int, A]})#IntState] {
    override def unit[A](a: => A): IntState[A] = State(s => (a, s))

    override def flatMap[A, B](ma: IntState[A])(f: A => IntState[B]): IntState[B] = ma.flatMap(f)
  }

  def stateMonad[S]: Monad[({type f[x] = State[S, x]})#f] = new Monad[({type f[x] = State[S, x]})#f] {
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] = ma.flatMap(f)


  }

  def getState[S]: State[S, S] = State(s => (s, s))

  def setState[S](s: => S): State[S, Unit] = State(_ => ((), s))

  /*
  exercise 11.18 答え
  `State`の` replicateM`は同じ状態遷移を何度も繰り返し、結果のリストを返します。
  同じ開始状態を何度も渡すのではなく、1つの出力状態が次の入力状態になるように呼び出しをチェーンします。

  `map2`は2つの状態遷移を取り、一方の出力状態をもう一方の入力に送るという点で同様に機能します。
  出力はリストには入れられず、関数 `f`と組み合わされます。

  `sequence`は状態遷移のリスト全体を受け取り、` replicateM`と同じ種類のことを行います。
  最初の状態遷移の出力状態を次の状態遷移の入力状態にフィードします。 結果はリストに蓄積されます。
   */

  /*
  //exercise 11.19 答え
  Getting and setting the same state does nothing:
  getState.flatMap(setState) == unit(())

  written as for-comprehension:
  for {
    x <- getState
    _ <- setState(x)
  } yield ()

  Setting the state to `s` and getting it back out yields `s`.
  setState(s).flatMap(_ => getState) == unit(s)

  alternatively:
  for {
    _ <- setState(s)
    x <- getState
  } yield x

  */

  val F = stateMonad[Int]

  def zipWIthIndex[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(F.unit(List[(Int, A)]()))((acc, a) => for {
      xs <- acc
      n <- getState
      _ <- setState(n + 1)
    } yield (n, a) :: xs).run(0)._1.reverse

  //モナドの規約は行間で何が起きるのかを指定するのではなく、何が起きようともそれが結合率と同一率を満たすことを明示するだけ
  //exercise 11.20

  /*
   意味の説明
   リーダーの `flatMap`のアクションは、` r`引数を外側のリーダーと内側のリーダーである `f`の結果の両方に渡すことです。
   「状態」が状態を渡す方法に似ていますが、「リーダー」では「状態」が読み取り専用であることを除きます。

   ここでの「シーケンス」の意味は、関数のリストがある場合、1つの引数を取り、
   それをリスト内のすべての関数に渡し、結果のリストを返す関数に変換できることです。

   `join`の意味は、両方の引数として同じ値をバイナリ関数に渡すことです。

  `replicateM`の意味は、同じ関数を同じ引数に何度も適用し、結果のリストを返すことです。
  この関数が_pure_（そうである必要があります）である場合、関数を何度も呼び出す代わりに、
  関数を1回だけ適用して結果を複製することでこれを利用できます。
  これは、ReaderモナドがreplicateMをオーバーライドして、非常に効率的な実装を提供できることを意味します。
   */

  case class Reader[R, A](run: R => A)

  object Reader {
    def readerMonad[R]: Monad[({type f[x] = Reader[R, x]})#f] = new Monad[({type f[x] = Reader[R, x]})#f] {
      def unit[A](a: => A): Reader[R, A] = ???

      def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = ???
    }
  }

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

    //exercise 11.14
    //join,map,unitの3つにのみ言及するようにモナド則を言い換えよ
    //問題の意味がわからない。理解することを放棄
    //15,16は飛ばす

    //monadのインスタンスは以下のいずれかの実装を提供しなければならない
    //unit, flatMap
    //unit, compose
    //unit, map, join
    //モナドとは、結合率と同一率を満たす最小限のモナドコビネータの集まりのいずれかを実装したもの

    //単位元monad
    val id1: Id[String] = Id("Hello, ").flatMap(s1 => Id("monad!").flatMap(s2 => Id(s1 + s2)))
    val id2: Id[String] = for {
      a <- Id("Hello, ")
      b <- Id("monad!")
    } yield a + b
    //モナドは変数を定義してバインドするためのコンテキストを提供し、変数置換を実行する


  }
}
