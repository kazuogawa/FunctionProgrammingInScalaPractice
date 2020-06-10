package Chapter6

object Chapter6 {

  trait RNG {
    //状態のカプセル化
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }

    def randomPair(rng: RNG): ((Int, Int), RNG) = {
      val (i1, rng2) = rng.nextInt
      val (i2, rng3) = rng2.nextInt
      ((i1, i2), rng3)
    }

    //exercise 6.1
    //https://www.scala-lang.org/api/2.12.0/scala/Int$.html
    //Int.MinValue(-2147483648)も対応できるようにするなら、符号をひっくり返すだけではダメ
    //Int.MaxValueは2147483647
    //streamで実装したが、rngが歪むからダメ？
    //def nonNegativeInt(rng: RNG): (Int, RNG) = Stream(rng.nextInt, rng.nextInt._2.nextInt).find(i => i._1 >= 0).get
    //そっか符号ひっくり返したあと、+1すればいいだけか
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (i, r) = rng.nextInt
      (if (i < 0) -(i + 1) else i, r)
    }

    //exercise 6.2
    //0 <= x < 1を生成
    //Int.MaxValueを使う？x.toDoubleでIntをDoubleに変換できる
    def double(rng: RNG): (Double, RNG) = {
      val (i, r) = nonNegativeInt(rng)
      (i.toDouble / Int.MaxValue, r)
    }

    //解答はこれ。ちょと違う
    def doubleAnswer(rng: RNG): (Double, RNG) = {
      val (i, r) = nonNegativeInt(rng)
      (i / (Int.MaxValue.toDouble + 1), r)
    }

    //exercise6.3
    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (i, nextRNG) = rng.nextInt
      val (d, _) = double(rng)
      ((i, d), nextRNG)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val ((i, d), nextRNG) = intDouble(rng)
      ((d, i), nextRNG)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d1, rng1) = double(rng)
      val (d2, rng2) = double(rng1)
      val (d3, rng3) = double(rng2)
      ((d1, d2, d3), rng3)
    }

    //exercise 6.4
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      @scala.annotation.tailrec
      def go(c: Int, l: List[Int], r: RNG): (List[Int], RNG) =
        if (c <= 0) (l, r) else {
          //整数(-も+も入る)なのでnegaはだめ
          val (i, nextRNG) = r.nextInt
          go(c - 1, i :: l, nextRNG)
        }

      go(count, List(), rng)
    }

    // RNGを使ってAを作成し、あとから別のアクションに利用できる新しい状態へRNGを遷移させる
    type Rand[+A] = RNG => (A, RNG)
    val int: Rand[Int] = _.nextInt

    //RNGの状態を明示的にやり取りするのを回避したRandのActionを結合するためのコンビネータ
    //ドメイン固有の言語のようなものになる
    //unitは常に同じ定数とrngを返す
    def unit[A](a: A): Rand[A] = rng => (a, rng)

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

    def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

    //exercise 6.5
    def doubleViaMap: Rand[Double] = map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

    //これでもOK
    //def doubleViaMap(rng: RNG): (Double, RNG) =
    //  map(nonNegativeInt)(_ / Int.MaxValue.toDouble + 1)(rng)

    //exercise6.6
    //Rand[Rand[C]]になるので、Randはずしたい.rngをどう合成するのか・・・
    //def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    //  map(ra)(a => map(rb)(b => f(a, b)))
    //答え。そっか、r1かr2だけとりだせばよかったのか
    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      (rng: RNG) => {
        val (a, r1) = ra(rng)
        val (b, r2) = rb(r1)
        (f(a, b), r2)
      }

    def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

    val randIntDouble: Rand[(Int, Double)] = both(int, double)
    val randDoubleInt: Rand[(Double, Int)] = both(double, int)

    //exercise 6.7
    //答え見た
    //foldRightか。。。unit使うのはなんでだろう・・・
    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
      fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

    //List.fill(n)(x)でlist作成できるらしい
    //こたえみた。すげえ。こんな単純にできるのか
    def intsViaSequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

    //0-nの整数を生成する
    //Int.MaxValueはnで割り切れないことがあるため、再帰的に呼び出し
    def nonNegativeLessThan(n: Int): Rand[Int] = { (rng: RNG) =>
      val (i, rng2) = nonNegativeInt(rng)
      val mod = i % n
      //わざわざ符号反転しているかもしれないことをこういう表現で表すのはどうなのか・・・
      if (i + (n - 1) - mod >= 0) (mod, rng2)
      else nonNegativeLessThan(n)(rng)
    }

    //exercise 6.8
    //答え見た。なんでこうなるの？根本をわかっていない可能性がある
    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
      val (a, r1) = f(rng)
      g(a)(r1) // We pass the new state along
    }

    //答え見た。なんでこうなるの？根本をわかっていない可能性がある
    def nonNegativeLessThanViaFlatMap(n: Int): Rand[Int] =
      flatMap(nonNegativeInt) { i =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
      }

    //exercise 6.9
    //全然わかっていないができた
    def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

    def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      flatMap(ra)(a => mapViaFlatMap(rb)(b => f(a, b)))

    //汎用化
    def mapGeneral[S, A, B](s: S => (A, S))(f: A => B): S => (B, S) = rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  //Randの汎用化
  //exercise 6.10
  //全滅。何もわからない
  //どこに書くかと型がわからない。そしてまたflatMapからかよ・・・
  case class State[S, +A](run: S => (A, S)) {
    def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })

    def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))


  }

  type Rand[A] = State[RNG, A]

  //コンパニオンオブジェクト...case classと同じ名前のオブジェクトで、
  // case class作った後にコンパニオンオブジェクトのメンバにアクセスできる
  object State {
    //型がわからなかったので答え見た
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))

    def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
      sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get
      _ <- set(f(s))
    } yield ()
  }

  //exercise 6.11
  sealed trait Input

  case object Coin extends Input

  case object Turn extends Input

  import State._

  case class Machine(locked: Boolean = true, candies: Int, coins: Int) {
    //答えみた。書き方わかったら条件はかけるが、、、、、
    def update: Input => Machine => Machine = (i: Input) => (s: Machine) =>
      (i, s) match {
        //candyがなかったら全て無視
        case (_, Machine(_, 0, _)) => s
        //ロックが外れた状態でコインをいれても変化なし
        case (Coin, Machine(false, _, _)) => s
        //ロックがかかっている状態でturnしても変化なし
        case (Turn, Machine(true, _, _)) => s
        //ロックがかかっている状態でコインを入れるとロックが外れる
        case (Coin, Machine(true, candy, coin)) => Machine(false, candy, coin + 1)
        //ロックがかかっていない状態でTurnするとcandyが減って、ロックされる
        case (Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin)
      }

    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
      _ <- sequence(inputs map (modify[Machine] _ compose update))
      s <- get
    } yield (s.coins, s.candies)
  }

  def main(args: Array[String]): Unit = {
    val rng = new scala.util.Random
    println(rng.nextDouble())
    println(rng.nextDouble())
    println(rng.nextInt())
    // 0-9のランダムな整数を取得
    println(rng.nextInt(10))

    val simpleRNG = SimpleRNG(42)
    val n1, simpleRNG2 = simpleRNG.nextInt
    println("n1")
    println(n1)
    println("simpleRNG2")
    println(simpleRNG2)

    //foldRight等で折りたたみ繰り返しで確認したかったが、やり方がわからなかった
    (1 to 100).foreach { i =>
      println(simpleRNG.nonNegativeInt(SimpleRNG(i)))
    }
    (1 to 100).foreach { i =>
      println(simpleRNG.double(SimpleRNG(i)))
    }
    println(simpleRNG.ints(10)(simpleRNG))

    val testMachine: Machine = Machine(candies = 5, coins = 10)
    testMachine.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))
  }
}
