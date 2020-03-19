package Chapter3

object Chapter3 extends App {

  //データ型はtraitキーワードを使って定義するのが一般的！
  //+は共変を表している
  sealed trait List[+A]

  //使用可能な形式を表すのにcaseで定義
  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      //case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    //exercise 3.1
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x //通らない
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y //ここ通るのでは？ 1 + 2で3
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    //exercise 3.2
    def tail[A](as: List[A]): List[A] = as match {
      case Nil => Nil //何もなかったらNilでいいのか？
      //case Nil => throw new Exception("List is Empty")
      case Cons(_, t) => t
    }

    //exercise 3.3
    def setHead[A](head: A, as: List[A]): List[A] = as match {
      case Nil => Cons(head, Nil)
      case Cons(_, t) => Cons(head, t)
    }

    //exercise 3.4
    @scala.annotation.tailrec
    def drop[A](l: List[A], n: Int): List[A] = l match {
      case Nil => Nil
      case _ if n <= 0 => l //match式の中に入れるべき？外がいい？
      case Cons(_, t) => drop(t, n - 1)
    }

    //exercise 3.5
    //日本語が難しい
    //@scala.annotation.tailrec
    def dropWhile2[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Cons(h, t) if f(h) => dropWhile2(t, f)
      case _ => l
    }

    //exercise 3.6
    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil //Nilでいいのか？
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

    //dropWhileのcurry化
    @scala.annotation.tailrec
    def dropWhile3[A](as: List[A])(f: A => Boolean): List[A] =
      as match {
        case Cons(h, t) if f(h) => dropWhile3(t)(f)
        case _ => as
      }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def sum2(ns: List[Int]): Int = foldRight(ns, 0)((x, y) => x + y)

    def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)

    //exercise 3.9
    def length[A](as: List[A]): Int = foldRight(as, 0)((_, counter) => counter + 1)

    //exercise 3.10
    //簡単である・・・
    @scala.annotation.tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

    //exercise 3.11
    def sumL(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

    def productL(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)

    def lengthL[A](as: List[A]): Int = foldLeft(as, 0)((counter, _) => counter + 1)

    //exercise 3.12
    def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((as, h) => Cons(h, as))

    //exercise 3.13
    //これでいいのかわからない.記述は可能なのでは？
    def foldLeftR[A, B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(as, z)((a, b) => f(b, a))

    def foldRightL[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(as, z)((b, a) => f(a, b))

    //exercise 3.14
    //なんでl1,l2はこの順なんだろう
    def append[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2)((hl, tl) => Cons(hl, tl))

    //exercise 3.15
    def concat[A](l: List[List[A]]): List[A] = foldLeft(l, Nil: List[A])(append)

    //exercise 3.16
    //こたえみた！mapってないのかー。なんでfoldLeftだとできないのだろう・・・
    def allPlus1(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

    //exercise 3.17
    def doubleToString(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

    //exercise 3.18
    def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))

    //exercise 3.19
    def filter[A](as: List[A])(f: A => Boolean): List[A] =
      foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

    //exercise 3.20
    //おもいつかなかった。こたえみた。すげえ
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
      concat(map(as)(f))

    //exercise 3.21
    //こたえみた
    def flatMapFilter[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else Nil)

    //exercise 3.22
    def ListValuePlus(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(l1h, l1t), Cons(l2h, l2t)) => Cons(l1h + l2h, ListValuePlus(l1t, l2t))
    }

    //exercise 3.23
    //こたえみた。どっからfでてきたの・・・？
    def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
    }

    val ex1: List[Double] = Nil
    val ex2: List[Int] = Cons(1, Nil)
    val ex3: List[String] = Cons("a", Cons("b", Nil))

    sealed trait Animal

    case object Dog extends Animal

    case object Cat extends Animal

    val animalList: List[Animal] = Cons(Dog, Cons(Cat, Nil))

    println("exercise 3.1")
    println(List.x)

    println("exercise 3.2")
    println(List.tail(ex1))
    println(List.tail(ex2))
    println(List.tail(ex3))

    println("exercise 3.3")
    println(List.setHead(1.2, ex1))
    println(List.setHead(200, ex2))
    println(List.setHead("zzz", ex3))

    println("exercise 3.4")
    println(List.drop(ex1, 1))
    println(List.drop(ex2, 1))
    println(List.drop(ex3, 1))
    val testList = List(1, 2, 3, 4, 5)
    println(List.drop(testList, 4))
    println(List.drop(testList, -4))
    println(List.drop(testList, 0))

    println("exercise 3.5")
    val conditions: Int => Boolean = (n: Int) => n < 3
    println(List.dropWhile2(Nil, conditions))
    println(List.dropWhile2(List(1, 2, 3, 4, 5), conditions))
    println(List.dropWhile2(List(1, 2), conditions))

    println("exercise 3.6")
    println(List.init(Nil))
    println(List.init(List(1)))
    println(List.init(List(1, 2, 3, 4, 5)))

    val xs: List[Int] = List(1, 2, 3, 4, 5)
    val dropWhileCurryResult: List[Int] = List.dropWhile3(xs)(x => x < 4)

    println(List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))

    println("exercise 3.9")
    println(List.length(Nil))
    println(List.length(List(1)))
    println(List.length(List(1, 2, 3, 4, 5)))

    println("exercise 3.10")

    println("exercise 3.11")
    println(List.sumL(List(1, 2, 3)))
    println(List.productL(List(2.0, 5.0, 3.0)))
    println(List.lengthL(List("aaa", "b", "ccc")))

    println("exercise 3.12")
    println(List.reverse(List("aaa", "b", "ccc")))

    println("exercise 3.13")
    println(List.foldLeftR(List(1, 2, 3), 0)(_ + _))
    println(List.foldRightL(List(1, 2, 3), 0)(_ + _))

    println("exercise 3.14")
    println(List.append(List(1, 2, 3), List(4, 5, 6)))

    println("exercise 3.15")
    println(List.concat(List(List(1, 2, 3), List(4, 5, 6))))

    println("exercise 3.16")
    println(List.allPlus1(List(1, 2, 3)))

    println("exercise 3.17")
    println(List.doubleToString(List(1.0, 2.0, 3.0)))

    println("exercise 3.18")
    println(List.map(List(1.0, 2.0, 3.0))(_ + 0.1))

    println("exercise 3.19")
    println(List.filter(List(1.0, 2.0, 3.0))(_ < 2.5))
  }
