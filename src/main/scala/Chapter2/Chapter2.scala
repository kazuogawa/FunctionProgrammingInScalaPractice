package Chapter2

object Chapter2 extends App {
  def fib(n: Int): Int = {
    @scala.annotation.tailrec
    def loop(key: Int, prev: Int, next: Int): Int =
      if (n == key) prev else loop(key + 1, next, prev + next)

    loop(0, 0, 1)
  }


  (1 to 10).foreach { i =>
    println("fib %d : %d".format(i, fib(i)))
  }

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int): String = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  def formatFactorial(n: Int): String = {
    val msg = "The factorial of %d is %d"
    msg.format(n, factorial(n))
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  //formatAbsみたいなのを単相関数(monomorphic function)という。1つの型のデータを操作する関数
  println(formatAbs(-42))
  println(formatFactorial(7))
  //上と同じ処理
  println(formatResult("absolute value", -42, abs))
  println(formatResult("factorial", 7, factorial))

  //指定されたkeyが含まれているインデックスを返す
  def findFirst(ss: Array[String], key: String): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)

    loop(0)
  }

  //findFirstの多相関数。相性関数とも呼ばれるらしい。
  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

    loop(0)
  }

  //EXERCISE2.2
  //指定された比較関数に従って、Array[A]がソートされているかどうか調べる関数
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @scala.annotation.tailrec
    def loop(ar: Array[A]): Boolean = {
      if (ar.length <= 1) true
      else if (ordered(ar(0), ar(1))) loop(ar.tail)
      else false
    }

    loop(as)
  }

  val ar1 = Array(3, 7, 11, 18, 29)
  println("sorted is " + isSorted(ar1, (i1: Int, i2: Int) => i1 < i2))
  println("sorted is " + isSorted(ar1, (i1: Int, i2: Int) => i1 > i2))

  val lessThan = new Function2[Int, Int, Boolean] {
    def apply(a: Int, b: Int): Boolean = a < b
  }

  val b = lessThan.apply(10, 20)
  println(b)

  //部分適用(partial application)
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)

  //EXERCISE2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)

  //EXERCISE2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  //戻り値に合わせてa,bを定義してfを使う感じね。
  //感覚は掴んだが、絶対忘れる・・・

  //これは簡単
  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
}