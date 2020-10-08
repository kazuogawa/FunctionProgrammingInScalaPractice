package Chapter13

import Chapter11.Chapter11.Monad

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

  sealed trait IO4[A] {
    def unit[A](a: => A): IO4[A] = new IO4[A] {
      def run = a
    }

    def apply[A](a: => A): IO4[A] = unit(a)

    def flatMap[B](f: A => IO4[B]): IO4[B] = FlatMap(this, f)

    def map[B](f: A => B): IO4[B] = flatMap(f andThen (Return(_)))

    def run[A](io: IO4[A]): A = io match {
      case Return(_) => _
      case Suspend(r) => r()
      case FlatMap(x, f) => x match {
        case Return(a) => run(f(a)) //aがAny
        case Suspend(r) => run(f(r())) //r()がAny
        case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f)) //aとfがAny
      }
    }
  }

  //余計なステップを実行せず直ちにAを返す。このコンストラクタを検出した時点でrunは計算が終了していることを認識する
  case class Return[A](a: A) extends IO4[A]

  //計算の中断。resumeは引数を受け取らない関数だが、なんらかの作用を持ち結果を返す
  case class Suspend[A](resume: () => A) extends IO4[A]

  //2つのステップの合成。flatMapを関数ではなく、データコンストラクタとして具体化する。これを検出した時点でrunは部分計算subを処理し、
  // subが結果を合成したところでkを継続する必要がある
  case class FlatMap[A, B](sub: IO4[A], k: A => IO4[B]) extends IO4[B]

  def printLine4(s: String): IO4[Unit] = Suspend(() => println(s))


  def main(args: Array[String]): Unit = {
    //convertor3.run
  }
}
