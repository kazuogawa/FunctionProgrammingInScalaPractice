package Chapter12

import java.util.Date

import Chapter10.Chapter10.{Foldable, Monoid}
import Chapter11.Chapter11.{Functor, Monad}
import Chapter6.Chapter6.State

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
    //def composeViaMonad[G[_]](G: Monad[G]): Monad[({type f[x] = F[G[x]]})#f] = {
    //  val self = this
    //  new Monad[({type f[x] = F[G[x]]})#f] {
    //    override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

    //    //できないらしい
    //    //override def flatMap[A, B](fa: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
    //    // self.flatMap(na => G.flatMap(na)(a => ???))
    //  }
    //}

    //アプリカティブファンクタを発見したのは、traverse関数とsequence関数、及びその他の演算がflatMapに直接依存しないことに気づいた結果
    //traverseとsequenceを再度一般化すればもう一つの抽象概念を発見できる
    //def traverse[F[_], A, B](as: List[A])(f: A => F[B]): F[List[B]]

    //def sequence[F[_], A](fas: List[F[A]]): F[List[A]]

    //Applicativeのような抽象インターフェースでListのような具体的な方コンストラクタが使用されていたときは
    // この型コンストラクタを抽象化したらどうなるかについてかんがえてみる
    //List以外のデータの一部はFoldable
    //List以外にもとらバーサブルなデータ型は存在する

    //exercise 12.12
    //Mapで実装
    //こたえみた。なるほどーってなった
    def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
      ofa.foldLeft(unit(Map[K, V]())) { case (acc, (k, fv)) =>
        map2(acc, fv)((m, v) => m + (k -> v))
      }

    //トラバーサブルなデータの数が多いことを考えると、それぞれに特化したsequence,traverseメソッドを記述するわけにはいかない
    //あたらしいinterfaceが必要
    trait Traverse[F[_]] {
      def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
        sequence(map(fa)(f))

      //Gがアプリカティブファンクタである限りFとGを入れ替える。これはかなり抽象的で代数的な概念
      def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] =
        traverse(fga)(ga => ga)
    }

    //exercise12.13
    //こたえみた。overrideするひつようがあるのか。どっちをoverrideするかってどうかんがえればいいのだろう。
    //今回はtraverseインスタンスだからtraverseのみ？
    //implicitが突然出てきたけど、何で？GはApplicative前提ってことを書きたい？
    val listTraverse: Traverse[List] = new Traverse[List] {
      override def traverse[G[_], A, B](as: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
        as.foldRight(G.unit(List[B]()))((a, fbs) => G.map2(f(a), fbs)(_ :: _))
    }

    val optionTraverse: Traverse[Option] = new Traverse[Option] {
      override def traverse[G[_], A, B](oa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
        oa match {
          case Some(a) => G.map(f(a))(Some(_))
          case None => G.unit(None)
        }
    }

    case class Tree[+A](head: A, tail: List[Tree[A]])

    //理解できなかったので、勉強会の時質問する
    val treeTraverse: Traverse[Tree] = new Traverse[Tree] {
      override def traverse[G[_], A, B](ta: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
        G.map2(f(ta.head), listTraverse.traverse(ta.tail)(a => traverse(a)(f)))(Tree(_, _))
    }


  }

  //sequenceの具体的な型シグネチャ
  //List[Option[A]] => Option[List[A]]
  //Traverse[List].sequenceの呼び出しは、ListのいずれかがNoneの場合にNoneを返す。それ以外の場合は元のListをSomeにラッピングして返す
  //Tree[Option[A]] => Option[Tree[A]]
  //Traverse[Tree].sequenceの呼び出しは、TreeのいずれかがNoneの場合にNone,それ以外の場合はTreeをSomeにラッピング
  //Map[K, Par[A]] => Par[Map[K,A]]
  //Traverse[Map[K, _]].sequenceの呼び出しは、マッピングの全ての値を同時に評価する並列計算を実行する
  //以上のことからsequence, traverseをベースとして驚くべき数の演算を可能な限り汎用的な方法で定義できることがわかる
  //トラバーサブルは何らかのデータの構造を受け取り、そのデータ構造に含まれているデータに関数を順番に適用して結果を生成するという点では畳み込みににている
  //ただし、traverseが元の構造を維持するのとは対照的に、foldMapは元の構造を削除してモノイドの演算と置き換える
  //例えばTree[Option[A] => Option[Tree[A]]はモノイドを使って値を破壊するのではなく、Tree構造を維持していることがわかる


  //flatMapに基づいてmap2のデフォルト実装を提供すればMonad[F]をApplicative[F]の部分型(subtype)にすることが可能

  trait Monad[F[_]] extends Applicative[F] {
    def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)

    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
      join(map(fa)(f))

    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
      a => flatMap(f(a))(g)

    override def map[A, B](fa: F[A])(f: A => B): F[B] =
      flatMap(fa)(a => unit(f(a)))

    override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
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
    override def sequence[A](lsa: List[Stream[A]]): Stream[List[A]] =
      lsa.foldRight(unit(List[A]()))((sa, sla) => map2(sa, sla)(_ :: _))
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


  //exercise 12.14
  //こたえみた
  //Applicativeを作成
  type Id[A] = A
  //monadの作成
  val idMonad: Monad[Id] = new Monad[Id] {
    def unit[A](a: => A): Id[A] = a

    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
  }

  //traveseSを実装するために下記Monadを記述
  object Monad {

    // Notice that in the case of a `Left`, flatMap does nothing.
    def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] =
      new Monad[({type f[x] = Either[E, x]})#f] {
        def unit[A](a: => A): Either[E, A] = Right(a)

        override def flatMap[A, B](eea: Either[E, A])(f: A => Either[E, B]) = eea match {
          case Right(a) => f(a)
          case Left(b) => Left(b)
        }
      }

    def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
      def unit[A](a: => A): State[S, A] = State(s => (a, s))

      override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
        st flatMap f
    }

    // Monad composition
    def composeM[G[_], H[_]](implicit G: Monad[G], H: Monad[H], T: Traverse[H]):
    Monad[({type f[x] = G[H[x]]})#f] = new Monad[({type f[x] = G[H[x]]})#f] {
      def unit[A](a: => A): G[H[A]] = G.unit(H.unit(a))

      override def flatMap[A, B](mna: G[H[A]])(f: A => G[H[B]]): G[H[B]] =
        G.flatMap(mna)(na => G.map(T.traverse(na)(f))(H.join))
    }

  }


  trait Traverse[F[_]] extends Functor[F] {
    def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] = sequence(map(fa)(f))

    def sequence[G[_], A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] = traverse(fga)(ga => ga)

    def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Id, A, B](fa)(f)(idMonad)


    def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
      traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)

    def zipWithIndex[A](ta: F[A]): F[(A, Int)] =
      traverseS(ta)((a: A) => for {
        i <- State.get[Int]
        _ <- State.set(i + 1)
      } yield (a, i)).run(0)._1

    def toList[A](fa: F[A]): List[A] =
      traverseS(fa)((a: A) => for {
        as <- State.get[List[A]]
        _ <- State.set(a :: as)
      } yield ()).run(Nil)._2.reverse

    def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
      traverseS(fa)((a: A) => for {
        s1 <- State.get[S]
        (b, s2) = f(a, s1)
        _ <- State.set(s2)
      } yield b).run(s)

    def toList[A](fa: F[A]): List[A] =
      mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

    def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
      mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

    //exercise 12.16
    //答え見た。挙動は謎・・・
    def reverse[A](fa: F[A]): F[A] =
      mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1


    //List, Tree, 他のトラバーサブルファンクタにとってそれが何を意味するのか考察せよ
    //答えなかった

    //全て下記を満たす
    //toList(reverse(x)) ++ toList(reverse(y)) == reverse(toList(y) ++ toList(x))

    //exercise 12.17
    //答え見た。思いつかない・・・
    def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B =
      mapAccum(fa, z)((a, b) => ((), f(b, a)))._2

    //形状が異なる引数は処理できない事に注意
    //FがListである場合Listの長さが異なる場合は処理できない
    def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      mapAccum(fa, toList(fb)) {
        case (_, Nil) => sys.error("zip: Incompatible shapes.")
        case (a, b :: bs) => ((a, b), bs)
      }._1

    //どちらか一方の引数の形状が優先されるバージョン

    def zipL[A, B](fa: F[A], fb: F[B]): F[(A, Option[B])] =
      mapAccum(fa, toList(fb)) {
        case (a, Nil) => ((a, None), Nil)
        case (a, b :: bs) => ((a, Some(b)), bs)
      }._1

    def zipR[A, B](fa: F[A], fb: F[B]): F[(Option[A], B)] =
      mapAccum(fb, toList(fa)) {
        case (b, Nil) => ((None, b), Nil)
        case (b, a :: as) => ((Some(a), b), as)
      }._1

    //exercise12.18
    //アプリカティブファンクタの積を使って2つの走査を融合。2つの関数f,gが与えられた時、faを1回操作し、f,gの関数の結果を1回で集める
    //答え見た。
    def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])
                              (G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = {
      traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G.product(H))
    }


    //exercise12.19
    //何も引数渡されてないじゃん・・・
    //答え見たが、selfが動かない
    def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = {
      val self = this
      new Traverse[({type f[x] = F[G[x]]})#f] {
        override def traverse[M[_] : Applicative, A, B](fa: F[G[A]])(f: A => G[B]): G[F[G[B]]] =
          self.traverse(fa)((ga: G[A]) => G.traverse(ga)(f))
      }
    }

    //exercise 12.20
    //def composeM[F[_], G[_]](F: Monad[F], G: Monad[G], T: Traverse[G]): Monad[({type f[x] = F[G[x]]})#f] =
    //  new Monad[({type f[x] = F[G[x]]})#f] {
    //    override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))

    //    //ここわからん
    //    override def flatMap[A, B](fga: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
    //      F.flatMap(fga)(ga => F.map(T.traverse(ga)(f))(G.join))
    //  }

    //答えと関数が違う・・・
    def composeM[G[_],H[_]](implicit G: Monad[G], H: Monad[H], T: Traverse[H]): Monad[({type f[x] = G[H[x]]})#f] =
      new Monad[({type f[x] = G[H[x]]})#f] {
      def unit[A](a: => A): G[H[A]] = G.unit(H.unit(a))
      override def flatMap[A,B](mna: G[H[A]])(f: A => G[H[B]]): G[H[B]] =
        G.flatMap(mna)(na => G.map(T.traverse(na)(f))(H.join))
    }

    //表現力や威力と引き換えに合成性とモジュール性が犠牲になる
    //モナドの合成は大抵合成用に構築されたカスタムモナドを使って解決する。それらはモナド変換子(monad transformer)と呼ばれる
    //例えば、OptionTモナド変換子はOptionを他のモナドと合成する

    //エラーになってる。。。。動くはずなのに
    //case class OptionT[M[_], A](value: M[Option[A]])(implicit M: Monad[M]){
    //  def flatMap[B](f: A => OptionT[M,B]): OptionT[M,B] =
    //    OptionT(value flatMap {
    //      case None => M.unit(None)
    //      case Some(a) => f(a).value
    //    })
    //}

  }


  //MonoidからApplicativeへの変換
  type Const[M, B] = M

  implicit def monoidApplicative[M](M: Monoid[M]): Applicative[({
    type f[x] = Const[M, x]
  })#f] =
    new Applicative[({
      type f[x] = Const[M, x]
    })#f] {
      override def unit[A](a: => A): Const[M, A] = M.zero

      override def map2[A, B, C](m1: M, m2: M)(f: (A, B) => C): M = M.op(m1, m2)

    }

  //TraverseをFoldableの拡張として定義できることと、traverseをベースとしてfoldMapのデフォルト実装を提供できることを意味する(わからん)

  //trait Traverse2[F[_]] extends Functor[F] with Foldable[F] {
  //  override def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M =
  //    traverse[(type f[x] = Const[M,x])#f, A, Nothing](as)(f)(monoidApplicative(mb))
  //}

  //TraverseがFoldableとFunctorの両方を拡張している
  //重要なのはFoldable自体はFunctorを拡張できないこと
  //foldをベースとしてmapを記述することは可能だが、普遍的に行うことは不可能

  //exercise 12.15
  //FoldableがFunctorを拡張できない理由を納得がいくまで検討せよ。ファンクタでないFoldableを思いつけるか？
  //答え見たけどわからない
  /*
  * これは、 `foldRight`、` foldLeft`、および `foldMap`では、折りたたみ可能なタイプの値を構築する方法が提供されないためです。
  * 構造に「マッピング」するには、新しい構造を作成する機能が必要です（「リスト」の場合は「Nil」と「Cons」など）。
  * トラバーサルは元の構造を維持するため、「トラバース」は「Functor」を正確に拡張できます。
    ファンクタではないFoldableの例：

     case class Iteration [A]（a：A、f：A => A、n：Int）{
       def foldMap [B]（g：A => B）（M：Monoid [B]）：B = {
         def iterate（n：Int、b：B、c：A）：B =
           if（n <= 0）b else iterate（n-1、g（c）、f（a））
         iterate（n、M.zero、a）
       }
     }

    このクラスは、いくつかのシード値から始まる関数の繰り返し適用によって生成された一連の「A」値を概念的に表します。
    * しかし、このタイプの「マップ」を定義できない理由を理解できますか？
  *
  *
  * */

  //Applicativeインスタンスは常に合成されますが、Monadインスタンスはそうではない

}
