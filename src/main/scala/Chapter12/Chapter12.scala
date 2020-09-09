package Chapter12

import java.util.Date

import Chapter11.Chapter11.{Functor, Monad}

object Chapter12 {

  //applicative functorを取り上げる章
  //Fは、ファンクタ。Listが入ったり、(A,B)などのTupleも入れたりできる便利な型コンストラクタ。
  //アプリカティブは、unitとflatmapや,unitとmap2などのプリミティブがあるかつ、F[_]のようにファンクタになっているもの？それはアプリカティブファンクタじゃね？
  //https://gist.github.com/kohyama/5856037
  trait Test[F[_]] {
    //unit,flatmap,mapは参考書には書いていないが、書いとかないとエラーになる
    def unit[A](a: => A): F[A]

    def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

    def map[A, B](ma: F[A])(f: A => B): F[B]

    def sequence[A](lfa: List[F[A]]): F[List[A]] =
      traverse(lfa)(fa => fa)

    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
      as.foldRight(unit(List[B]()))((a: A, mbs: F[List[B]]) => map2(f(a), mbs)(_ :: _))

    def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
      flatMap(ma)(a => map(mb)(b => f(a, b)))
  }

  trait Applicative[F[_]] extends Functor[F] {
    //プリミティブコンビネータ
    //map2ViaUnitAndApplyに実装したのでそれを使う
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = map2ViaUnitAndApply(fa, fb)(f)

    def unit[A](a: => A): F[A]

    //派生コンビネータ
    //map2でmapを実装するのか・・・意味あるのか・・？
    def map[A, B](fa: F[A])(f: A => B): F[B] =
      map2(fa, unit(()))((a, _) => f(a))

    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
      as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

    //exercise 12.1
    //map2とunit、あるいはそれらに基づいて実装されるメソッドでsequence, replicateM,productを作る
    def sequence[A](fas: List[F[A]]): F[List[A]] =
      traverse(fas)(fa => fa)

    def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
      sequence(List.fill(n)(fa))

    //productのfbの型まちがっている...
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    //map2(fa, fb)((a, b) => (a, b))
    //答え見た。簡略化できたね・・
      map2(fa, fb)((_, _))

    //アプリカティブという名前は、別のプリミティブ(unitとapply)を使ってApplicativeインターフェースを表現できることに由来
    //exercise 12.2
    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)(_ (_))

    //unitとapplyでmap2とmapを定義
    //答え見た。map使ってんじゃん・・・curry化して、A => B => Cにした後に
    // F[A]はfaで確定してF[B => C]をmapは返す。
    // applyのfabにF[B => C]を入れて、faにfbを入れるのか
    def map2ViaUnitAndApply[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    //map(fa)(f.curried) : F[B => C]
      apply(map(fa)(f.curried))(fb)


    def mapViaUnitAndApply[A, B](fa: F[A])(f: A => B): F[B] =
      apply(unit(f))(fa)

    //map2とunitを使ってapplyを定義
    //答え見た。なにこれ？勉強会の時に質問する
    def applyViaMap2AndUnit[A, B](fab: F[A => B])(fa: F[A]): F[B] =
      map2(fab, fa)(_ (_))

    //exercise 12.3
    //unit, apply, curriedの3つのメソッドのみを使ってmap3,map4を実装せよ
    def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
      apply(apply(apply(unit(f.curried))(fa))(fb))(fc)


    def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
      apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

    //exercise 12.8
    //答え見た
    def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
      val self = this
      new Applicative[({type f[x] = (F[x], G[x])})#f] {
        def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

        override def apply[A, B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])): (F[B], G[B]) =
          (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))

      }
    }

    //exercise 12.9
    def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
      val self = this
      new Applicative[({type f[x] = F[G[x]]})#f] {
        override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

        //答え見た。composeとproductで最低限実装すべき物が違うのか・・・
        override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C): F[G[C]] =
          self.map2(fga, fgb)(G.map2(_, _)(f))
      }
    }

    //exercise 12.10
    //このってどれ？上二つとも？
    //理解するのを放棄した

    //exercise 12.11
    //monadでcomposeを記述できないことを試す
    def composeViaMonad[G[_]](G: Monad[G]): Monad[({type f[x] = F[G[x]]})#f] = {
      val self = this
      new Monad[({type f[x] = F[G[x]]})#f] {
        override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

        //できないらしい
        //override def flatMap[A, B](fa: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
        // self.flatMap(na => G.flatMap(na)(a => ???))
      }
    }
  }


  //flatMapに基づいてmap2のデフォルト実装を提供すればMonad[F]をApplicative[F]の部分型(subtype)にすることが可能

  trait Monad[F[_]] extends Applicative[F] {
    def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)

    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
      join(map(fa)(f))

    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
      a => flatMap(f(a))(g)

    override def map[A, B](fa: F[A])(f: A => B): F[B] =
      flatMap(fa)(a => unit(f(a)))

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      flatMap(fa)(a => map(fb)(b => f(a, b)))

  }

  //今のところはAPIの関数を整理して型シグネチャにしたがって実装しているだけ・・・笑

  //map2とunitの組み合わせはモナドの最小限の演算ではない。下記joinを実装することができない
  //def join[A](f: F[F[A]]): F[A]

  //ApplicativeとMonadの相違点
  //Applicative: 計算の構造が固定,コンテキストに依存しない
  //Monad: 前の作用の結果に基づいて構造を動的に選択できる, コンテキストに依存させることができる, 作用はファーストクラス(事前に選択されるのではなく解釈時に生成できる

  //じゃあなんでApplicativeがあるの？重要な点は？
  //アプリカティブファンク他は合成されるが、モナドは合成されない

  //streamのapplicativeはmap2, unitは定義できるがflatMapは定義できない

  val streamApplicative: Applicative[Stream] = new Applicative[Stream] {
    override def map2[A, B, C](fa: Stream[A], fb: Stream[B])(f: (A, B) => C): Stream[C] =
      fa.zip(fb).map(f.tupled)

    //無限の定数ストリーム
    override def unit[A](a: => A): Stream[A] = Stream.continually(a)

    //exercise 12.4
    //sequenceにどのような意味があるか
    //Listにまとめられる？ってこと？
    override def sequence[A](a: List[Stream[A]]): Stream[List[A]] = super.sequence(a)
  }
  //exercise 12.5

  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {
    //override def unit[A](a: => A): Either[E, A] = Either(_ => a)
    //答え見た。そっかunitだからRightしか返さないのか・・・
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ma match {
      case Right(value) => f(value)
      //case _ => _
      //答えではこうなっているが上記でもOK?
      case Left(e) => Left(e)
    }
  }

  //flatMapとmap3の違い
  //Either[String, T]の型を使用していることを前提とする
  //validNameがエラーで失敗した場合validBirthdateとvalidPhoneは実行すらされない
  //validName(field1) flatMap (f1 =>
  //  validBirthdate(field2) flatMap (f2 =>
  //    validPhone(filed3) map (f3 => WebForm(f1,f2,f3))
  //    )
  //  )
  //map3には3つの式に依存関係があることは明示されていない。
  //各EitherのエラーをListに集めそうなイメージだが、map3がflatMapで実装されている場合は、1つ目のエラーで停止してしまう
  //map3(
  //  validName(field1),
  //  validBirthdate(filed2),
  //  validPhone(filed3))(WebForm(_, _, _)
  //)

  sealed trait Validation[+E, +A]

  case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]

  case class Success[A](a: A) extends Validation[Nothing, A]

  //exercise 12.6
  //答えみた。インスタンスって言ってたからval ~~~ = new ~~~かと思ったらdefだった
  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] =
    new Applicative[({type f[x] = Validation[E, x]})#f] {
      //これはEitherMonadと一緒な感じ
      def unit[A](a: => A): Success[A] = Success(a)

      //やっぱmatch caseするしかないのか・・・
      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
        (fa, fb) match {
          case (Success(a), Success(b)) => Success(f(a, b))
          case (Failure(h1, t1), Failure(h2, t2)) =>
            Failure(h1, t1 ++ Vector(h2) ++ t2)
          //e@ってなんだっけ・・・？Failureだったらそれをeに入れるってこと？
          case (e@Failure(_, _), _) => e
          case (_, e@Failure(_, _)) => e
        }

    }

  case class WebForm(name: String, birthdate: Date, phoneNumber: String) {
    //これらの関数ってcase classの中がいいのか、そとがいいのか。trait作ってvalidateにまとめるのがいいのか
    def validName(name: String): Validation[String, String] =
      if (name != "") Success(name)
      else Failure("Name cannot be empty")

    def validBirthdate(birthdate: String): Validation[String, Date] =
      try {
        import java.text._
        Success(new SimpleDateFormat("yyyy-MM-dd").parse(birthdate))
      } catch {
        //参考書はcaseがない
        case _: Exception => Failure("Birthdate must be in the form yyyy-MM-dd")
      }

    def validPhone(phoneNumber: String): Validation[String, String] =
      if (phoneNumber.matches("[0-9]{10}"))
        Success(phoneNumber)
      else Failure("Phone number numst be 10 digits")

    //map3の実装どうするの？
    //def validWebForm(name: String, birthdate: String, phone: String): Validation[String, WebForm] =
    //  map3(validName(name), validBirthdate(birthdate),
    //    validPhone(phone))(WebForm)
  }

  //ファンクタ則
  //map(v)(id) == v
  //map(map(v)(g))(f) == map(v)(f compose g)

  //アプリカティブファンクタはmapの実装ベースがmap2とunitであることから、他の法則として
  //def map[A, B](fa: F[A])(f: A => B): F[B] =
  ////右単位元の法則
  //  map2(fa, unit())((a, _) => f(a))

  ////これでもいい
  //def map[A, B](fa: F[A])(f: A => B): F[B] =
  ////左単位元の法則
  //  map2(unit(), fa)((_, a) => f(a))

  //次にmap3について
  //def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D]
  //上記シグネチャより
  //op(a, op(b, c) == op(op(a, b), c)
  //compose(f, op(g,h) == compose(compose(f, g),h)
  //アプリカティブファンク他の結合則も全体の考え方は同じ
  //上記の法則がなかればどっちから関数を当てるかをわけるmap3L,map3Rのようなことをしないといけない。
  //def product[A, B](fa: F[A], F[B]): F[A, B] =
  //  map2(fa,fb)((_, _))
  def assoc[A, B, C](p: (A, (B, C))): ((A, B), C) =
    p match {
      case (a, (b, c)) => ((a, b), c)
    }

  //productコンビネータとassocコンビネータをを使用した場合、アプリカティブファンクタの結合則は下記になる
  //左は左結合、右は右結合になっているのに注意
  //product(product(fa, fb), fc) == map(product(fa, product(fb, fc)))(assoc)
  val F: Applicative[Option] = new Applicative[Option] {
    override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
      fa.flatMap(a => fb.map(b => f(a, b)))

    override def unit[A](a: => A): Option[A] = Some(a)
  }

  case class Employee(name: String, id: Int)

  case class Pay(rate: Double, hoursPerYear: Double)

  def format(e: Option[Employee], pay: Option[Pay]): Option[String] =
    F.map2(e, pay) { (e, pay) =>
      s"${e.name} makes ${pay.rate * pay.hoursPerYear}"
    }

  //val e: Option[Employee] = ???
  //val pay: Option[Pay] = ???
  //format(e, pay)

  def formatStringDouble(e: Option[String], pay: Option[Double]): Option[String] = {
    F.map2(e, pay) { (e, pay) => s"$e makes $pay" }
  }

  //自然性の法則
  //map2(a, b)(productF(f,g)) == product(map(a)(f), map(b)(g))
  //2つの関数を1つの関数に結合し、両方の引数を受け取り、結果をペアで返す
  def productF[I, O, I2, O2](f: I => O, g: I2 => O2): (I, I2) => (O, O2) =
    (i, i2) => (f(i), g(i2))

  //アプリカティブの法則はunit, map, map2の動作に一貫性と合理性があることを保証する

  //exercise 12.7
  //全てのモナドがアプリカティブファンクタであることを証明せよ
  //答え見た
  //map2(unit(), fa)((_, a) => a) == fa
  //map2(fa, unit())((_, a) => a) == fa
  //map2の拡張
  //def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => F[C]) =
  //  flatMap(fa)(a => map(fb)(b => f(a, b)))

  // unit()と(_, a) => aを代入
  //  flatMap(unit())(u => map(fa)(a => a)) == fa
  // a => aは単にfaなので
  //  flatMap(unit())(u => fa) == fa
  // 最初の関数の引数として「Unit」を使用することにより、「flatMap」は「compose」を使用して書き換えることができることを思い出してください。
  // compose(unit, u => fa)(()) => fa
  // 簡略可能なので
  // (u => fa)(()) == fa
  // fa == fa
  // まとめるとこんなかんじ
  //    flatMap(fa)(a => map(unit(()))(u => a)) == fa
  //    flatMap(fa)(a => unit(a)) == fa  // via functor laws
  //    compose(u => fa, unit)(()) == fa
  //    (u => fa)(()) == fa
  //    fa == fa


}
