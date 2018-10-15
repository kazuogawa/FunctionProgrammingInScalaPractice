object Practice {
  def main(args: Array[String]): Unit = {
    val testArray = Array(1,2,3,4)
    println(exercise2_2.isSorted(testArray,(x:Int , y:Int) => x > y))
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

  object exercise2_5 {
    def compose[A,B,C](f:B => C, g:A => B):A => C = {
      (a:A) => f(g(a))
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
    def dropWhile[A](l:List[A], f:A => Boolean): List[A] = {
      @annotation.tailrec
      def loop[A](loopList:List[A]):List[A] = {
        l match {
          case Nil          => Nil
          case head :: tail => if(f(head)) tail else loop(tail)
        }
      }
      loop(l)
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
    def sum(l:List[Int]):Int = {
      exercise_3_10.foldLeft(l, 0)(_ + _)
    }
    //def sum(l:List[Double]):Double = {
    //  exercise_3_10.foldLeft(l, 0.0)(_ + _)
    //}
  }
}
