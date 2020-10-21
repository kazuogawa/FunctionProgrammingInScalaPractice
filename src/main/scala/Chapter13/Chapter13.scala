package Chapter13

import Chapter11.Chapter11.Monad
import Chapter11.Par
import Chapter11.Par.Par

import scala.annotation.tailrec

object Chapter13 {

  //作用(effect)と副作用(side effect)は意味が違う
  //EDSL(Embedded domain-specific Language)の作成に必要なスキルを身に着ける

  case class Player(name: String, score: Int)

  //結果を表示するためのIOコードが商社を割り出すための純粋なロジックと結び付けられている。
  def contest(p1: Player, p2: Player): Unit =
    if (p1.score > p2.score)
      println(s"${p1.name} is the winner!")
    else if (p2.score > p1.score)
      println(s"${p2.name} is the winner!")
    else
      println("It's a draw.")

  //わける
  def winner(p1: Player, p2: Player): Option[Player] =
    if (p1.score > p2.score) Some(p1)
    else if (p2.score > p1.score) Some(p2)
    else None

  //winnerの計算するところを分離した
  def contest2(p1: Player, p2: Player): Unit = winner(p1, p2) match {
    case Some(Player(name, _)) => println(s"$name is the winner!")
    case None => println("It's a draw.")
  }

  //純粋でないプロシージャ(複数の処理をまとめたもの, 手続きとも表現する)は純粋なコアの関数と副作用を持つ二つのプロシージャに常に分解できます

  //さらにわける
  def winnerMessage(p: Option[Player]): String = p map {
    case Player(name, _) => s"$name is ther winner!"
  } getOrElse ("It's a draw.")

  //副作用を持つ関数の中にヒア必ず純粋関数として抽出できる部分があることがわかる
  def contest3(p1: Player, p2: Player): Unit = println(winnerMessage(winner(p1, p2)))

  //A => B型の非純粋関数fがあるとすれば、fをい可能二つの関数に分割できる
  //A => D型の純粋関数: Dはfの結果に関する記述
  //D => B型の非純粋関数: これらの記述のインタープリタとして考えることができる

  trait IO {
    def run: Unit
  }

  def PrintLine(msg: String): IO =
    new IO {
      def run: Unit = println(msg)
    }

  //これで純粋関数になった。IO型の値を返すが、この値は実行する必要があるアクションを説明するだけでそれを実際に実行するわけではない
  //作用を持つ(または作用を発生させる)ため、エフェクトフルと言えるが、実際に副作用を発生するのはIOのインタープリタ、つまりrunメソッドだけ。
  //この関数はプログラムの各部分を合成させるやくわりをもっているだけ。
  //この作用を解釈して実際にコンソールを操作する責任はIOのrunメソッドにある
  def contest4(p1: Player, p2: Player): IO =
    PrintLine(winnerMessage(winner(p1, p2)))

  //Monoid
  trait IO2 {
    self =>
    def run: Unit

    //結合演算
    def ++(io: IO2): IO2 = new IO2 {
      def run: Unit = {
        self.run
        io.run
      }
    }
  }

  object IO2 {
    //単位元
    def empty: IO2 = new IO2 {
      def run = ()
    }
  }

  //List[IO2]があれば、畳み込んで単一のIOに簡約ができる

  def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0 / 9.0

  def convertor: Unit = {
    println("Enter a temperature in degrees Fahrenheit: ")
    val d = readLine.toDouble
    println(fahrenheitToCelsius(d))
  }

  //readLineのStringを保持する場所がないのでかけない
  //def converter: IO = {
  //
  //}

  sealed trait IO3[A] {
    self =>
    def run: A

    def map[B](f: A => B): IO3[B] = new IO3[B] {
      def run: B = f(self.run)
    }

    def flatMap[B](f: A => IO3[B]): IO3[B] = new IO3[B] {
      def run: B = f(self.run).run
    }
  }

  object IO3 extends Monad[IO3] {
    def unit[A](a: => A): IO3[A] = new IO3[A] {
      def run = a
    }

    override def flatMap[A, B](fa: IO3[A])(f: A => IO3[B]): IO3[B] = fa flatMap f

    //IOぶろっくを生成可能になる
    def apply[A](a: => A): IO3[A] = unit(a)

  }

  def ReadLine: IO3[String] = IO3 {
    readLine
  }

  def PrintLine3(msg: String): IO3[Unit] = IO3 {
    println(msg)
  }

  //副作用がなくなっている。エフェクトフルな計算に対する参照透過な記述
  //convertor3.runで作用を実際に実行する
  def convertor3: IO3[Unit] = for {
    _ <- PrintLine3("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine3(fahrenheitToCelsius(d).toString)
  } yield ()

  //その他のユースケース
  val echo: IO3[Nothing] = ReadLine.flatMap(PrintLine3)
  val readInt: IO3[Int] = ReadLine.map(_.toInt)

  //**が動かない
  //val readInts: IO3[(Int, Int)] = readInt ** readInt
  //コンソールから10行読み取り、結果をリストで返すIO
  //replicateM(10)(ReadLine)

  //入力された値の階乗を求める
  //なんとなく読める
  //def factorial(n: Int): IO3[Int] = for {
  //  acc <- ref(1)
  //  _ <- foreachM(1 to n toStream)(i => acc.modify(_ * i).skip)
  //  result <- acc.get
  //} yield result
  //val factorialREPL: IO3[Unit] = sequence_(
  //  IO3 {
  //    println(helpstring)
  //  },
  //  doWhile {
  //    IO {
  //      readLine
  //    }
  //  } { line =>
  //    val ok: Boolean = line != "q"
  //    when(ok)
  //    for {
  //      n <- factorial(line.toInt)
  //      _ <- IO {
  //        println("factorial: " + n)
  //      }
  //    } yield ()
  //  }
  //)

  //sealed trait IO4[A] {
  //  def unit[A](a: => A): IO4[A] = new IO4[A] {
  //    def run = a
  //  }

  //  def apply[A](a: => A): IO4[A] = unit(a)

  //  def flatMap[B](f: A => IO4[B]): IO4[B] = FlatMap(this, f)

  //  def map[B](f: A => B): IO4[B] = flatMap(f andThen (Return(_)))

  //  //コルーチン(一旦処理を中断した後、続きから処理を再開するやつ)のような物らしい
  //  //トランポリンともいう。制御を一つのループに戻すことでスタックを排除するテクニック全体をトランポリン化という。
  //  //TODO: 再起処理にcase classのSuspendのようなものが挟まればトランポリン？
  //  //@annotation.tailrec
  //  //def run[A](io: IO4[A]): A = io match {
  //  //  case Return(_) => _
  //  //  case Suspend(r) => r()
  //  //  //run(f(run(x))にもできるが、そうした場合、runが末尾にこないためxにmatchを使っている TODO: よくわからないので質問する
  //  //  case FlatMap(x, f) => x match {
  //  //    case Return(a) => run(f(a)) //aがAny
  //  //    case Suspend(r) => run(f(r())) //r()がAny
  //  //    case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f)) //aとfがAny
  //  //  }
  //  //}
  //  ////余計なステップを実行せず直ちにAを返す。このコンストラクタを検出した時点でrunは計算が終了していることを認識する
  //  //case class Return[A](a: A) extends IO4[A]

  //  ////計算の中断。resumeは引数を受け取らない関数だが、なんらかの作用を持ち結果を返す
  //  //case class Suspend[A](resume: () => A) extends IO4[A]

  //  ////2つのステップの合成。flatMapを関数ではなく、データコンストラクタとして具体化する。これを検出した時点でrunは部分計算subを処理し、
  //  //// subが結果を合成したところでkを継続する必要がある
  //  //case class FlatMap[A, B](sub: IO4[A], k: A => IO4[B]) extends IO4[B]

  //  //エラーになる
  //  // メリット:スタックオーバーフローは発生しなくなる
  //  // デメリット: どのような作用が発生するかわからない。実行スレッドをブロックせずにIOを実行するための並列化の手段がない
  //  //sealed trait TailRec[A] {
  //  //  def flatMap[B](f: A => TailRec[B]): TailRec[B] =
  //  //    FlatMap(this, f)

  //  //  def map[B](f: A => B): TailRec[B] =
  //  //    flatMap(f andThen (Return(_)))
  //  //}

  //  //case class Return[A](a: A) extends TailRec[A]

  //  //case class Suspend[A](resume: () => A) extends TailRec[A]

  //  //case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

  //  //FlatMap(Suspend(s), k)を調べ、s()を呼び出し、runに制御を戻し、sを実行して結果を待ち、得られた値をkに渡す


  //  //非同期処理をサポートするようになったので、任意のParを受け取るSuspendコンストラクタを使って、埋め込むことができる
  //  //sealed trait Async[A] {
  //  //  def flatMap[B](f: A => Async[B]): Async[B] =
  //  //    FlatMap(this, f)

  //  //  def map[B](f: A => B): Async[B] =
  //  //    flatMap(f andThen (Return(_)))
  //  //}

  //  //case class Return[A](a: A) extends Async[A]

  //  //case class Suspend[A](resume: Par[A]) extends Async[A]

  //  //case class FlatMap[A, B](sub: Async[A], k: A => Async[B]) extends Async[B]

  //  //def step[A](async: Async[A]): Async[A] = async match {
  //  //  case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
  //  //  case FlatMap(Return(x), f) => step(f(x))
  //  //  case _ => async
  //  //}

  //  //def run[A](async: Async[A]): Par[A] = step(async) match {
  //  //  case Return(a) => Par.unit(a)
  //  //  case Suspend(r) => r
  //  //  case FlatMap(x, f) => x match {
  //  //    case Suspend(r) => Par.flatMap(r)(a => run(f(a)))
  //  //    case _ => sys.error("Impossible; `step` eliminates these cases")
  //  //  }
  //  //}


  //}

  //ParすらFに置き換える
  sealed trait Free[F[_], A] {
    //exercise 13.1
    def map[B](f: A => B): Free[F, B] = FlatMap(this, f andThen (Return(_)))

    def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)

    def freeMonad[F[_]]: Monad[({type f[A] = Free[F, A]})#f] = new Monad[({type f[A] = Free[F, A]})#f] {
      override def unit[A](a: => A): Free[F, A] = Return(a)

      //ここだけ答え見た
      override def flatMap[A, B](ma: Free[F, A])(f: A => Free[F, B]): Free[F, B] = ma.flatMap(f)
    }

    //exercise 13.2
    //@annotation.tailrec
    //def runTrampoline[A](a: Free[Function0, A]): A =
    //  a match {
    //    case Return(_) => _
    //    case Suspend(s) => s()
    //    case FlatMap(s, f) => s match {
    //        //答え見た。同じのを思ったが、Anyになるので動かないと思った
    //      case Return(a) => runTrampoline { f(a) }
    //      case Suspend(r) => runTrampoline { f(r()) }
    //      case FlatMap(a0,g) => runTrampoline { a0 flatMap { a0 => g(a0) flatMap f } }
    //    }
    //  }

    //exercise 13.3
    //stepがあっているかはわからない
    def step[F[_], A](a: Free[F, A]): Free[F, A] = a match {
      case FlatMap(FlatMap(s, f), g) => step(s flatMap (a => f(a) flatMap g))
      case FlatMap(Return(a), f) => step(f(a))
      case _ => a
    }

    //stepを書き忘れてた
    def run[F[_], A](a: Free[F, A])(implicit F: Monad[F]): F[A] = step(a) match {
      case Return(aa) => F.unit(aa)
      case Suspend(r) => r
      case FlatMap(x, f) => x match {
        case Suspend(r) => F.flatMap(r)(aa => run(f(aa)))
        case _ => sys.error("error")
      }
    }
  }

  case class Return[F[_], A](a: A) extends Free[F, A]

  case class Suspend[F[_], A](s: F[A]) extends Free[F, A]

  case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

  type TailRec[A] = Free[Function0, A]
  type Async[A] = Free[Par, A]


  //def printLine4(s: String): IO4[Unit] = Suspend(() => println(s))

  //val f: Int => IO4[Int] = (x: Int) => Return(x)
  //val g = List.fill(10000)(f).foldLeft(f) {
  //  (a, b) => x => Suspend(() => ()).flatMap { _ => a(x).flatMap(b) }
  //}

  // TODO: runがない。。。runってIOの外に書くやつ？
  //val x1 = run(g(0))

  //Free[F,A]は型Aの値が0こ以上のFのレイヤに包含された再帰構造
  //Function0[A]は値がどのようなものになるかを推論できない


  sealed trait Console[A] {
    def toPar: Par[A]

    //FUnction0[A]として解釈
    def toThunk: () => A
  }

  case object ReadLine extends Console[Option[String]] {
    override def toPar: Par[Option[String]] = Par.lazyUnit(run)

    override def toThunk: () => Option[String] = () => run

    def run: Option[String] =
      try Some(readLine())
      catch {
        case e: Exception => None
      }
  }

  case class PrintLine(line: String) extends Console[Unit] {
    override def toPar: Par[Unit] = Par.lazyUnit(println(line))

    override def toThunk: () => Unit = () => println(line)
  }

  //ConsoleはReadLineかPrintLineのどちらかのみ返す
  object Console {
    type ConsoleIO[A] = Free[Console, A]

    def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)

    def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))

  }

  val f1: Free[Console, Option[String]] = for {
    _ <- Console.printLn("I can only interact with the console.")
    ln <- Console.readLn
  } yield ln

  //Console型をFunction0やParなどのモナドを形成する他の型に変換しなければならない
  trait Translate[F[_], G[_]] {
    def apply[A](f: F[A]): G[A]
  }

  type ~>[F[_], G[_]] = Translate[F, G]

  val consoleToFunction0: Console ~> Function0 = new (Console ~> Function0) {
    override def apply[A](f: Console[A]): () => A = f.toThunk
  }

  val consoleToPar: Console -> Par = new (Console ~> Par) {
    override def apply[A](f: Console[A]): Par[A] = f.toPar
  }

  //runの実装を一般化
  //F ~> G型の値を受け取り、F[F,A]プログラムの解釈時に変換を実行する
  //これをすると、Free[Console,A]をFunciton0[A]またはPar[A]のどちらかに変換する便利な関数runConsoleFunction0とrunConsoleParを実装できるらしい
  def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(implicit G: Monad[G]): G[A] =
    step(free) match {
      case Return(a) => G.unit(a)
      case Suspend(r) => t(r)
      case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _ => sys.error("Impossible; `step` eliminates there cases")
    }

  implicit val function0Monad: Monad[Function0] = new Monad[Function0] {
    override def unit[A](a: => A): () => A = () => a

    override def flatMap[A, B](ma: () => A)(f: A => () => B): () => B = () => f(ma())()
  }

  def runConsoleFunciton0[A](a: Free[Console, A]): () => A = runFree[Console, Function0, A](a)(consoleToFunction0)

  def runConsolePar[A](a: Free[Console, A]): Par[A] = runFree[Console, Par, A](a)(consoleToPar)


  def main(args: Array[String]): Unit = {
    //convertor3.run

  }
}
