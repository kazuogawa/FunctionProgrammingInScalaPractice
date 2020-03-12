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
      case Cons(0.0, _) => 0.0
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
    @scala.annotation.tailrec
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
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
    def dropWhile[A](as: List[A])(f: A => Boolean): List[A] =
      as match {
        case Cons(h, t) if f(h) => dropWhile(t)(f)
        case _ => as
      }
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
  println(List.dropWhile(Nil, conditions))
  println(List.dropWhile(List(1, 2, 3, 4, 5), conditions))
  println(List.dropWhile(List(1, 2), conditions))

  println("exercise 3.6")
  println(List.init(Nil))
  println(List.init(List(1)))
  println(List.init(List(1, 2, 3, 4, 5)))

  val xs: List[Int] = List(1, 2, 3, 4, 5)
  val tochu = List.dropWhile(xs)
  val dropWhileCurryResult: List[Int] = tochu(x => x < 4)

}
