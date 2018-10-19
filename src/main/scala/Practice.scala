import scala.collection.immutable.List
object Practice {
  def main(args: Array[String]): Unit = {
    val testArray = Array(1,2,3,4)
    println(exercise2_2.isSorted(testArray,(x:Int , y:Int) => x > y))
    println("length is " + exercise_3_9.length(List("1231","12312")))
  }

  object exercise1 {
    case class CreditCard(num:Int, pass:String)
    class Coffee {
      val price:Int = 400
    }

    case class Charge(creditCardInfo:CreditCard, amount:Int){
      def combine(other:Charge):Charge =
        if (creditCardInfo == other.creditCardInfo) Charge(creditCardInfo, amount + other.amount)
        else throw new Exception("can't combine charge to different cards")
      def coalesce(charges:List[Charge]):List[Charge] =
        charges.groupBy(_.creditCardInfo).values.map(_.reduce(_ combine _)).toList
    }

    class Cafe{
      def buyCoffee(creditcardInfo:CreditCard): (Coffee, Charge) = {
        val cup = new Coffee
        (cup, Charge(creditcardInfo,cup.price))
      }

      def buyCoffees(creditCardInfo: CreditCard, n:Int):(List[Coffee], Charge) = {
        //purchases・・購入、買い込む
        //fill・・第一引数の個数で第二引数を埋めたListを返す
        val purchases:List[(Coffee,Charge)] = List.fill(n)(buyCoffee(creditCardInfo))
        //unzip・・第一引数、第二引数のListをそれぞれ作って返す
        val (coffees: List[Coffee],charges: List[Charge]) = purchases.unzip
        //reduce・・foldの初期値ないバージョン
        (coffees,charges.reduce((charge1,charge2) => charge1.combine(charge2)))
      }
    }
  }

  object exercise2_2 {
    def isSorted[A](as:Array[A], ordered:(A,A) => Boolean):Boolean = {
      @annotation.tailrec
      def loop(n:Int,acc:Boolean):Boolean = {
        if ((n + 1) >= as.length)
          acc
        else
          loop(n + 1, acc && ordered(as(n), as(n + 1)))
      }
      loop(0, acc = true)
    }
  }

  def partial1[A,B,C](a:A, f:(A,B) => C):B => C = (b:B) => f(a,b)

  object exercise2_3 {
    def curry[A,B,C](f:(A,B) => C): A => B => C = (a:A) => (b:B) => f(a,b)
  }

  object exercise2_4 {
    def uncurry[A,B,C](f:A => B => C):(A, B) => C = (a:A,b:B) => f(a)(b)
  }

  object exercise2_5 {
    def compose[A,B,C](f:B => C, g:A => B):A => C = {
      a:A => f(g(a))
    }
  }

  object exercise3_2 {
    def tail[A](list:List[A]):List[A] = list match {
      case Nil    => Nil
      case _ :: t => t
    }
  }

  object exercise3_3{
    def setHead[T](l: List[T],p:T):List[T] = {
      l match {
        case Nil => List(p)
        case _ :: tail => p :: tail
      }
    }
  }

  object exercise_3_4 {
    @annotation.tailrec
    def drop[A](l:List[A], n:Int):List[A] = {
      if(n <= 0) l
      else l match {
        case Nil => Nil
        case head :: tail => drop(tail, n - 1)
      }
    }
  }

  object exercise_3_5 {
    def dropWhile[A](l:List[A], f:A => Boolean): List[A] =
      l match {
        case h :: t if f(h) => dropWhile(t, f)
        case _          => l
      }

    def dropWhileCurry[A](l:List[A])(f:A => Boolean):List[A] =
      l match {
        case h :: t if f(h) => dropWhileCurry(t)(f)
        case _          => l
      }

  }

  object P48{
    def foldRight[A,B](as:List[A], z: B)(f: (A, B) => B):B = as match {
      case Nil => z
      case h :: t => f(h, foldRight(t,z)(f))
    }
    def sum(l:List[Int]):Int = foldRight(l,0)(_ + _)
    def product(l:List[Int]):Int = foldRight(l,1)(_ * _)
  }

  object exercise_3_9 {
    def length[A](as:List[A]):Int = {
      as.foldRight(0)((_,acc) => acc + 1)
    }
  }

  object exercise_3_10{
    @annotation.tailrec
    def foldLeft[A,B](as:List[A], z:B)(f:(B, A) => B):B = {
      as match {
        case Nil          => z
        case head :: tail => foldLeft(tail, f(z,head))(f)
      }
    }
  }

  object exercise_3_11 {
    def sum(l:List[Int]):Int = exercise_3_10.foldLeft(l, 0)(_ + _)

    //def sum(l:List[Double]):Double = {
    //  exercise_3_10.foldLeft(l, 0.0)(_ + _)
    //}
  }
}
