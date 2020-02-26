import Practice.sample5_2.exercise6.Rand

import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.util.Try
object Practice {
  def main(args: Array[String]): Unit = {
    /*
    val testArray = Array(1,2,3,4)
    println(exercise2_2.isSorted(testArray,(x:Int , y:Int) => x > y))
    println("length is " + exercise_3_9.length(List("1231","12312")))

    import list_3_4.{Branch,Leaf}
    val tree = Branch(Branch(Branch(Leaf(2), Leaf(5)),Leaf(7)),Branch(Leaf(3), Leaf(9)))
    println("maximum: " + list_3_4.exercise_3_26.maximum(tree))
    println("depth: " + list_3_4.exercise_3_27.depth(tree))
    println(list4_3.mean(List(0,0)))
    sample5_1.aTest
    sample5_1.xTest
    */
    println(sample5_2.Stream.ones.take(5).toList)
    println(sample5_2.Stream.constant(5).take(5).toList)
    println(sample5_2.Stream.from(5).take(5).toList)
    println(sample5_2.Stream.fibs.take(5).toList)
    println(sample5_2.exercise6.rollDie(sample5_2.exercise6.SimpleRNG(5))._1)
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
    def product(l:List[Int]):Int = exercise_3_10.foldLeft(l,1)(_ * _)
    //def sum(l:List[Double]):Double = {
    //  exercise_3_10.foldLeft(l, 0.0)(_ + _)
    //}
  }

  object exercise_3_12 {
    def reverse[A](l:List[A]):List[A] = {
      l.foldLeft(List[A]())((acc, h) => h :: acc)
    }
  }

  //わかんね
  object exercise_3_13 {
  //  def foldLeft[A,B](l:List[A], z:B)(f:(B, A) => B):B = l.foldRight(z)((a,b) => f(b, a))
  }

  //わかんね
  object exercise_3_15 {
  //  def flatten[A](l:List[List[A]]):List[A] = {
  //    l.foldLeft[A](List[A]())((a, acc) => )
  //  }
  }

  object exercise_3_16 {
    def listIncrement(l:List[Int]):List[Int] = l.map(_ + 1)
  }

  object list_3_4 {
    sealed trait Tree[+A]
    case class Leaf[A](value: A) extends Tree[A]
    case class Branch[A](left:Tree[A], right:Tree[A]) extends Tree[A]

    object exercise_3_25{
      def treeSize[A](tree:Tree[A]):Int = tree match {
        case Leaf(_) => 1
        case Branch(l, r) => 1 + treeSize(l) + treeSize(r)
      }
    }
    object exercise_3_26 {
      def maximum(tree:Tree[Int]):Int = tree match {
        case Leaf(value) => value
        case Branch(left, right) => maximum(left) max maximum(right)
      }
    }
    object exercise_3_27 {
      def depth[A](tree:Tree[A]):Int = {
        def counter[A](t:Tree[A], num:Int):Int = t match {
          case Leaf(_) => num
          case Branch(left, right) => 1 + (counter(left, num) max counter(right, num))
        }
        counter(tree,0)
      }
    }
    object exercise_3_28 {
      def map[A,B](tree:Tree[A])(f: A => B):Tree[B] = tree match {
        case Leaf(value) => Leaf(f(value))
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      }
    }
    object exrcise_3_29 {
      sealed trait Tree[+A]
      case class Leaf[A](value: A) extends Tree[A]
      case class Branch[A](left:Tree[A], right:Tree[A]) extends Tree[A]

      private def fold[A,B](l:Tree[A])(f1: A => B)(f2: (B,B) => B):B = l match {
        case Leaf(value) => f1(value)
        case Branch(left, right) => f2(fold(left)(f1)(f2), fold(right)(f1)(f2))
      }

      def maximum(tree:Tree[Int]):Int = fold(tree)(a => a)(_ max _)
      def depth[A](tree:Tree[A]):Int = fold(tree)(_ => 0)((d1,d2) => 1 + (d1 max d2))
      def map[A,B](tree:Tree[A])(f: A => B):Tree[B] =
        fold(tree)(a => Leaf(f(a)):Tree[B])(Branch(_,_))
    }
  }

  object list4_1 {
    def failingFn(i:Int):Int = {
      val y: Int = throw new Exception("fail")
      try {
        val x = 42 + 5
        x + y
      } catch {
        case e: Exception => 43
      }
    }
  }

  object list4_3 {
    sealed trait Option[+A]
    case class Some[+A](get:A) extends Option[A]
    case object None extends Option[Nothing]
    def mean(xs:List[Double]):Option[Double] = if(xs.isEmpty) None else Some(xs.sum / xs.length)

  }
  object exercise4_1 {
    case class Some[+A](get:A) extends Option[A]
    case object None extends Option[Nothing]
    trait Option[+A] {
      def map[B](f:A => B): Option[B] = this match {
        case None => None
        case Some(value) => Some(f(value))
      }
      def flatMap[B](f:A => Option[B]): Option[B] = map(f).getOrElse(None)
      // => Bって書き方なんだっけ・・・
      def getOrElse[B >: A](default: => B):B = this match {
        case None => default
        case Some(value) => value
      }
      def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
        case None => ob
        case Some(value) => Some(value)
      }
      def filter(f:A => Boolean): Option[A] = this match {
        case Some(value) if f(value) => this
        case None => None
      }
    }

  }

  object exercise4_2 {
    def variance(xs: Seq[Double]): Option[Double] = {
      if (xs.isEmpty) None
      else {
        val m = xs.sum / xs.length
        val ansList = xs.map(x => math.pow(x - m, 2))
        Some(ansList.sum / xs.length)
      }
    }
  }

  object exercise4_3 {
    def map2[A,B,C](a:Option[A], b:Option[B])(f: (A,B) => C):Option[C] = (a,b) match {
      case (Some(valueA), Some(valueB)) => Option(f(valueA,valueB))
      case _ => None
    }

    def ansmap2[A,B,C](a:Option[A], b:Option[B])(f: (A,B) => C):Option[C] =
      a flatMap (aa => b map (bb => f(aa, bb)))
  }

  object exercise4_4 {
    def seqence[A](a:List[Option[A]]):Option[List[A]] = if(a.contains(None)) None else Some(a.map(p => p.get))
  }

  //object exercise4_5 {
  //  def traverse[A,B](a:List[A])(f:A => Option[B]):Option[List[B]] =
  //}

  object sampleMap{
    def map2_1[A,B,C](a:Option[A], b: Option[B])(f:(A, B) => C):Option[C] =
      a flatMap (aa => b map (bb => f(aa, bb)))
    //上記の内包表記
    def map2_2[A,B,C](a:Option[A], b: Option[B])(f:(A, B) => C):Option[C] =
      for {
        aa <- a
        bb <- b
      } yield f(aa,bb)
  }

  object list4_6{
    sealed trait Either[+E,+A]
    case class Left[+E](value: E) extends Either[E, Nothing]
    case class Right[+A](value:A) extends Either[Nothing, A]
    object list4_7 {
      def mean(xs:IndexedSeq[Double]):Either[String,Double] =
        if(xs.isEmpty) Left("mean of empty list")
        else Right(xs.sum / xs.length)
    }
    object list4_8 {
      def safeDiv(x:Int, y:Int):Either[Exception, Int] =
        try Right(x / y)
        catch {case e:Exception => Left(e)}
    }
    object list4_9 {
      def Try[A](a: => A):Either[Exception, A] = try Right(a) catch {case e:Exception => Left(e)}
    }
  }

  object exercise4_6 {
    case class Left[+E](value: E) extends Either[E, Nothing]
    case class Right[+A](value:A) extends Either[Nothing, A]
    trait Either[+E, +A]{
      def map[B](f:A => B):Either[E,B] = this match{
        case Right(x) => Right(f(x))
        case Left(e)  => Left(e)
      }

      def flatMap[EE >:E, B](f:A => Either[EE,B]):Either[EE,B] = this match {
        case Right(x) => f(x)
        case Left(e) => Left(e)
      }

      //TODO >:などの記号が理解できていない。共変・反変・上限境界・下限境界
      def orElse[EE  >:E, B >: A](b:Either[EE,B]):Either[EE,B] = this match{
        case Right(x) => Right(x)
        case Left(_) => b
      }
      //TODO:理解不能。何がどうしてそうなった
      def map2[EE    >:E, B, C](b:Either[EE,B])(f:(A,B) => C):Either[EE,C]= for {
        aa <- this
        bb <- b
      } yield f(aa,bb)

      object exercise4_7 {
        def traverse[E, A, B](as:List[A])(f:A => Either[E,B]):Either[E,List[B]] = as match {
          case Nil => Right(Nil)
          case h::t => (f(h) map2 traverse(t)(f))(_ :: _)
          //なぜドットはダメで、スペースはOKなのか
          //case h :: t => (f(h).map2.traverse(t)(f))(_ :: _)
        }
        //なぜそういう実装になるのか不明
        def sequence[E, A](es:List[Either[E,A]]):Either[E,List[A]] = traverse(es)(x => x)
      }

      //Eitherを使った実装
      case class Person(name:Name, age:Age)
      sealed class Name(val value:String)
      sealed class Age(val value:Int)

      def mkName(name:String):Either[String,Name] =
        if(name == "" || name == null) Left("name is empty")
        else Right(new Name(name))
      def mkAge(age:Int):Either[String,Age] =
        if(age < 0) Left("Age is out of range") else Right(new Age(age))

      //for式の条件に合わなかったら、Left吐くみたいな実装でいいね
      def mkPerson(name:String,age:Int):Either[String,Person] =
        mkName(name).map2(mkAge(age))(Person)
    }
  }
  object sample5_1{
    //遅延評価をするために => Aとしている
    def if2[A](cond:Boolean, onTrue: => A, onFalse: => A):A = if(cond) onTrue else onFalse
    def aTest = {
      val a = 20
      if2(a < 22, println("a"), println("b"))
    }
    //デフォルトでは、1度評価したものはキャッシュされない
    def maybeTwice(b:Boolean, i: => Int) = if (b) i + i else 0
    def maybeTwice2(b:Boolean, i: => Int) = {
      lazy val j = i
      if(b) j + j else 0
    }
    def xTest = maybeTwice(true, {println("hi"); 1 + 41})
    def xTest2 = maybeTwice2(true, {println("hi"); 1 + 41})
  }

  object sample5_2 {
    //https://github.com/astorije/fpinscala-exercises/blob/master/src/main/scala/ch5laziness/Stream.scala
    trait Stream[+A] {
      //強制評価
      def headOption:Option[A] = this match {
        case Empty => None
        case Cons(h, _) => Some(h())
      }
      //Exercise 5.1
      def toList:List[A] = this match {
        case Empty => Nil
        case Cons(h, t) => h() :: t().toList
      }
      //Exercise 5.2
      def take(n:Int):Stream[A] = this match {
        case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
        case Cons(h, _) if n == 1 => cons(h(), Empty)
        case Empty => Empty
      }
      def drop(n:Int):Stream[A] = this match {
        case Cons(_, t) if n <= 0 => t()
        case Cons(_, t) => t().drop(n - 1)
        case Empty => Empty
      }
      def cons[A](hd: => A, tl: => Stream[A]):Stream[A] = {
        //評価の繰り返しを避けるために遅延値としてキャッシュ
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)
      }
      //Exercise 5.3
      def takeWhile(p: A => Boolean):Stream[A] = this match {
        case Cons(h , t) if p(h()) =>  cons(h(), t().takeWhile(p))
        case Empty => Empty
      }

      def exists(p:A => Boolean):Boolean = this match {
        // ||を使うことで、p(h())が先に評価されてtrueになったら、||のあとは評価されない。すごい
        // しかもconsで末尾が評価されるはずだったものが、評価されなくなるのがもっとすごい。
        case Cons(h, t) => p(h()) || t().exists(p)
        case _ => false
      }

      def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
        case Cons(h,t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }

      //Exercise5_4
      def forAll(p: A => Boolean):Boolean = this match {
        case Cons(h, _) if !p(h()) => false
        //上でfalseでなければtrueになるはずなので、forallにtailを渡すだけでok
        case Cons(_, t) => t().forAll(p)
        //空になったらtrueを返す
        case _ => true
      }

      //Exercise5_5
      def takeWhileViaFoldRight(p: A => Boolean):Stream[A] = foldRight[Stream[A]](Empty)((h, t) => if(p(h)) cons(h,t) else Empty)

      //Exercise5_6
      //なぜfoldRightでheadOptionなのか・・・
      def headOptionViaFoldRight:Option[A] = foldRight[Option[A]](None)((h, _) => Some(h))

      //Emptyが通らねえ。。
      //def mapViaFoldRight[B](f:A => B):Stream[B] = foldRight(Empty[B])((h, t) => cons(f(h), t))
      //def filterViaFoldRight(p: A => Boolean):Stream[A] = foldRight(Empty[A])((h, t) => if(p(h)) cons(h,t) else t)
      def appendViaFoldRight[B >: A](s:Stream[B]):Stream[B] = foldRight(s)((h, t) => cons(h, t))
    }
    case object Empty extends Stream[Nothing]
    //空ではないストリームは先頭と末尾で構成される。それらは非性格(すぐに評価されない)である。明示的な強制を必要とするサンクらしい
    case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

    object Stream {
      //空では無いストリームを作成するためのスマートコンストラクタ
      def cons[A](hd: => A, tl: => Stream[A]):Stream[A] = {
        //評価の繰り返しを避けるために遅延値としてキャッシュ
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)
      }
      //特定の型の空のストリームを作成するためのスマートコンストラクタ
      def empty[A]:Stream[A] = Empty

      //複数の要素からStreamを作成するための可変長の引数を持つ便利なメソッド
      def apply[A](as:A*):Stream[A] = if(as.isEmpty) empty else cons(as.head,apply(as.tail: _*))
      //sample 5_4
      val ones:Stream[Int] = Stream.cons(1,ones)
      //exercise 5_8
      def constant[A](a:A):Stream[A] = Stream.cons(a, constant(a))
      //exercise 5_9
      def from(n:Int):Stream[Int] = Stream.cons(n, from(n + 1))
      //exercise 5.10
      def fibs:Stream[Int] = {
        def loop(provious:Int, current:Int):Stream[Int] = cons(provious, loop(current, provious + current))
        loop(0, 1)
      }
      //exercise 5_11
      //unfoldが何なのか理解できなかった。謎
      def unfold[A, S](z: S)(f: S => Option[(A,S)]):Stream[A] = f(z) match {
        case None => Empty
        case Some((a, s)) => cons(a, unfold(s)(f))
      }

      //exercise 5_12
      def fibsViaUnfold:Stream[Int] = unfold((0, 1)){
        case (i, j) => Some((i, (j, i + j)))
      }
      def fromViaUnfold(n:Int):Stream[Int] = unfold(n)(i => Some((i, i + 1)))
      def constantViaUnfold[A](a:A):Stream[A] = unfold(a)(_ => Some((a, a)))
      def onesViaUnfold:Stream[Int] = unfold(1)(_ => Some((1,1)))
      //exercise 5_13から次は、unfoldを理解していないので、今度やる
    }

    object exercise6{
      trait RNG{
        def nextInt:(Int, RNG)
      }
      //scala.util.Randomと同じアルゴリズムを使用する純粋関数型の乱数ジェネレータ
      //中身は理解しなくてもいい
      case class SimpleRNG(seed:Long) extends RNG{
        def nextInt:(Int, RNG) = {
          val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFL
          val nextRNG = SimpleRNG(newSeed)
          val n = (newSeed >>> 16).toInt
          (n, nextRNG)
        }

      }
      //exercise 6.1
      //
      //https://github.com/fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/state/State.scala
      //intの範囲2147483647, -2147483648	なので、負数だったら1加算する
      def nonNegativeInt(rng:RNG):(Int,RNG) = {
        val (nextInt, nextRNG) = rng.nextInt
        (if(nextInt < 0) -(nextInt + 1) else nextInt , nextRNG)
      }

      //exercise 6.2
      //MaxValueまで出てくるので、それで割れば、1以下にはなるはず。。。
      def double(rng:RNG):(Double, RNG) = {
        val (nextInt, nextRNG) = rng.nextInt
        (nextInt / (Int.MaxValue.toDouble + 1), nextRNG)
      }

      //exercise 6.3
      def intDouble(rng:RNG):((Int, Double), RNG) = {
        val (nextInt   , nextRNG1) = nonNegativeInt(rng)
        val (nextDouble, nextRNG2) = double(nextRNG1)
        ((nextInt,nextDouble), nextRNG2)
      }
      def doubleInt(rng:RNG):((Double, Int), RNG) = {
        val ((i, d), r) = intDouble(rng)
        ((d, i), r)
      }
      def double3(rng:RNG):((Double, Double, Double), RNG) = {
        val (d1 , rng1) = double(rng)
        val (d2 , rng2) = double(rng1)
        val (d3 , rng3) = double(rng2)
        ((d1,d2,d3), rng3)
      }
      //exercise 6.4
      def ints(count:Int)(rng:RNG):(List[Int], RNG) =
        if(count == 0) (Nil, rng)
        else {
          val (nextInt1, nextRng1) = rng.nextInt
          val (nextInt2, nextRng2) = ints(count - 1)(nextRng1)
          (nextInt1 :: nextInt2, nextRng2)
        }
      //RNGを遷移させるプログラムらしい
      type Rand[+A] = RNG => (A, RNG)
      val int: Rand[Int] = _.nextInt

      //どういう表現なのだこれは
      def unit[A](a:A):Rand[A] = rng => (a, rng)

      //状態そのものを変化させずに、状態アクションの出力を変換するmap
      def map[A,B](s:Rand[A])(f: A => B):Rand[B] =
        rng => {
          val (a, rng2) = s(rng)
          (f(a), rng2)
        }

      def nonNegativeEven:Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

      //exercise 6.5
      //謎
      def doubleViaMap:Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

      //exercise 6.6
      //全くもって謎
      def map2[A,B,C](ra:Rand[A], rb:Rand[B])(f:(A, B) => C):Rand[C] =
        rng => {
          val (a, r1) = ra(rng)
          val (b, r2) = rb(r1)
          (f(a,b), r2)
        }
      def both[A,B](ra:Rand[A], rb:Rand[B]):Rand[(A,B)] = map2(ra,rb)((_,_))
      val randIntDouble:Rand[(Int, Double)] = both(int, double)
      val randDoubleInt:Rand[(Double, Int)] = both(double, int)

      //exercise 6.7
      def sequence[A](fs:List[Rand[A]]):Rand[List[A]] =
        fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

      def nonNegativeLessThan(n:Int):Rand[Int] = {rng =>
        val (i, rng2) = nonNegativeInt(rng)
        val mod = i % n
        if(i + (n - 1) - mod >= 0)
          (mod, rng2)
        else nonNegativeLessThan(n)(rng)
      }

      def flatMap[A,B](f:Rand[A])(g:A => Rand[B]):Rand[B] = {rng =>
        val (nextInt, nextRng:RNG) = f(rng)
        //なぜこういう渡し方になるのかがわからない・・
        g(nextInt)(nextRng)
      }

      //exercise 6.9
      def mapViaFlatMap[A,B](s:Rand[A])(f: A => B):Rand[B] = flatMap(s)(a => unit(f(a)))
      def map2ViaFlatMap[A,B,C](ra:Rand[A], rb:Rand[B])(f:(A, B) => C):Rand[C] =
        flatMap(ra)(a => map(rb)(b => f(a,b)))

      def rollDie:Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
      def mapBeta[S,A,B](a:S => (A,S))(f: A => B):S => (B, S) = ???
    }

  }

  object part2{
    object list7{
      //これだと直列処理
      def sumViaFoldLeft(l:List[Int]):Int = l.foldLeft(0)((acc, i) => acc + i)
      //これだと2分割されながらの並列処理
      def sumViaSpintsitAt(ints:List[Int]):Int = {
        if(ints.size <= 1)
          ints.headOption.getOrElse(0)
        else{
          val (leftInts, rightInts) = ints.splitAt(ints.length/2)
          sumViaSpintsitAt(leftInts) + sumViaSpintsitAt(rightInts)
        }
      }

      import java.util.concurrent._
      import language.implicitConversions
      //結果を保存するPar型を作成
      object Par {

        type Par[A] = ExecutorService => Future[A]
        //評価されていないAを受け取り、別のスレッドで評価する結果を返す
        //定数値を並列計算に昇格させる
        def unit[A](a: => A): Par[A] = ???
        //並列計算から結果を返す
        //実際に計算して、結果を返す
        def run[A](a:Par[A]): A = ???

        def sum(ints:List[Int]):Int = {
          if(ints.size <= 1)
            ints.headOption.getOrElse(0)
          else{
            val (leftInts, rightInts) = ints.splitAt(ints.length/2)
            val sumL:Par[Int] = Par.unit(sum(leftInts))
            val sumR:Par[Int] = Par.unit(sum(rightInts))
            Par.run(sumL) + Par.run(sumR)
          }
        }

        //2つの並列計算の結果を2項関数で結合する
        def map2[A,B,C](leftPar:Par[A],rightPar:Par[B])(f: (A, B) => C):Par[C] = {
          //なんか冗長で気持ち悪い
          Par.unit(f(Par.run(leftPar), Par.run(rightPar)))
        }

        def sumReturnPar(ints:List[Int]):Par[Int]= {
          if(ints.size <= 1)
            Par.unit(ints.headOption.getOrElse(0))
          else{
            val (leftInts, rightInts) = ints.splitAt(ints.length/2)
            Par.map2(sumReturnPar(leftInts), sumReturnPar(rightInts))(_ + _)
          }
        }

        //計算をメインスレッドから分岐させるタイミングを表現する式
        //計算を並列評価の対象としてマークする。この評価はrunによって
        //強制されるまで実際には発生しない
        def fork[A](p: => A):Par[A] = ???

        //def sumViaIndexSeq(l:IndexedSeq[Int]):Par[Int] =
        //  if(l.length <= 1) Par.unit(l.headOption.getOrElse(0))
        //  else {
        //    val (leftIndexSeq, rightIndexSeq) = l.splitAt(l.length / 2)
        //      Par.map2(
        //        Par.fork(sumViaIndexSeq(leftIndexSeq)),
        //        Par.fork(sumViaIndexSeq(rightIndexSeq))
        //      )(_ + _)
        //  }

        //評価されて引数をParでラッピングして、並列評価の対象としてマークする
        def lazyUnit[A](a: => A):Par[A] = fork(a)
      }
    }
  }

}
