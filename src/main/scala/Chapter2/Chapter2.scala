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
}