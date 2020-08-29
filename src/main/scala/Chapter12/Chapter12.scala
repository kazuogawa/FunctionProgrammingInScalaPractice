package Chapter12

import Chapter11.Chapter11.Functor

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
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

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
    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]

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

      override def apply[A, B](fab: Stream[A => B])(fa: Stream[A]): Stream[B] = ???

      //exercise 12.4
      //sequenceにどのような意味があるか
      //Listにまとめられる？ってこと？
      override def sequence[A](a: List[Stream[A]]): Stream[List[A]] = super.sequence(a)
    }
  }

}
