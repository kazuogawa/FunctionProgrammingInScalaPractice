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
          val (i, nextRNG) = nonNegativeInt(r)
          go(c - 1, i :: l, nextRNG)
        }

      go(count, List(), rng)
    }
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
  }
}
