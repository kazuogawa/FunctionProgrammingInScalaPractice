package Chapter4

import scala.util.Random

object Chapter4 {

  def main(args: Array[String]): Unit = {
    def failingFn(i: Int): Int = {
      //これだとエラーになる
      //val y:Int = throw new Exception("fail")
      try {
        val x = 42 + 5
        //x + y
        x + ((throw new Exception("fail")): Int)
      }
      catch {
        case _: Exception => 43
      }
    }

    println(failingFn(12))

    //部分関数(partial function) = 一部の入力に関して定義されない関数
    //http://www.cs.tsukuba.ac.jp/~kam/lecture/discrete2011/text/3.pdf
    //def mean(xs: Seq[Double]): Double =
    ////0.0を返すのは、バグが外で発生するかもしれないからダメ
    //  if (xs.isEmpty) throw new ArithmeticException("mean of empty list")
    //  else xs.sum / xs.length

    //Emptyの時の値を外で指定できるようにすることもできるが、計算を途中で止めたい時とか、別の分岐に入って欲しい時に自由に処理ができなくなる
    def mean_1(xs: IndexedSeq[Double], onEmpty: Double): Double =
      if (xs.isEmpty) onEmpty
      else xs.sum / xs.length

    //sealed trait Option[+A]

    case class Some[+A](get: A) extends Option[A]
    case object None extends Option[Nothing]

    def meanOpt(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    sealed trait Option[+A] {
      //exercise 4.1
      //this使うの思いつかなかった
      def map[B](f: A => B): Option[B] = this match {
        case Some(a) => Some(f(a))
        //Noneの方がいい？
        case _ => None
      }

      //こたえみた。それなら先にgetOrElse書いて欲しかった・・・
      def flatMap[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)

      def getOrElse[B >: A](default: => B): B = this match {
        case Some(a) => a
        case _ => default
      }

      //パターンマッチ以外の方法わからん
      def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
        case None => ob
        case Some(a) => Some(a)
      }

      //答え見た。ようこんなんおもいつくなぁ。。。
      def filter(f: A => Boolean): Option[A] = this match {
        case Some(a) if f(a) => this
        case None => None
      }

      //これも思いつかない。みたらへぇ。確かに。となる。というかflatMapに他の関数噛ませたものを渡すとかかんがえつかん
      //this.map(f)でSome(Some(a))ってなってるところにgetOrElseでSome(a)になるとか・・・はあぁ・・・・すご。。。
      def filter_1(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)

      //exercise 4.1終了


    }

    case class Employee(name: String, department: String)

    //def lookupByName(name: String): Option[Employee] = ???
    //これしたかったが、Someが既存のものとかぶっていて名前解決できないっぽくて無理みたい
    //def lookupByName(name: String): Option[Employee] = if (Random.nextBoolean) Some(Employee("Joe", "Accounting")) else None
    //val joeDepartment: Option[String] = lookupByName("Joe").map(_.department)


    //exercise 4.2
    //答え見た。meanつくるのかよ。同名の関数増えすぎて困る
    def mean(xs: Seq[Double]): Option[Double] = if (xs.nonEmpty) Some(xs.sum / xs.length) else None

    //flatmapに渡すものをさらにmapして外側にmeanするとかおもいつかん・・・
    def variance(xs: Seq[Double]): Option[Double] =
      mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

    //val dept: String = lookupByName("Joe").map(_.department).filter(_ != "Accounting").getOrElse("Default Dept")

    def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

    //Optionの中身があれば、Optionで包まれた状態で絶対値に変換
    def abs0: Option[Double] => Option[Double] = lift(math.abs)

    //保険料計算。変更不可なのでOptionとかでつつめない。ってことにしておくらしい
    def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???

    def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
      val optAge: Option[Int] = Try(age.toInt)
      val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
      map2(optAge, optTickets)(insuranceRateQuote)
    }

    def Try[A](a: => A): Option[A] = try (Some(a)) catch {
      case e: Exception => None
    }

    //exercise 4.3
    //答え見た。へえぇってなった。
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      a.flatMap(aa => b.map(bb => f(aa, bb)))
    //これでもOK?
    //a.flatMap(aa => b.flatMap(bb => Some(f(aa, bb))))

  }
}
