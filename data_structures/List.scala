// sealed: Listトレイト（抽象インタフェース）の実装がこのファイルで宣言される
// +A: Aが共変パラメータである=部分型を許容する
// 今回はNothing（全ての型の部分型）を使うため
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

// コンパニオンオブジェクト: 上記データ型&データコンストラクタを操作するための関数
object List {
    def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x,xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x,xs) => x * product
    }
    // sum と product の一般化
    // カリー化もする
    // つまり，引数を別グループにすることで型情報が左から右へと引き継がれていく
    // つまり，関数を呼び出すときに型情報を与えなくても推論される
    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
    // foldRightを使ったバージョン
    def sum2(ns: List[int]) =
        foldRight(ns, 0)((x,y) => x + y) //関数がカリー化されていると関数を渡すときに型を指定しなくて良い
    def product2(ns: List[Double]) =
        foldRight(ns, 1.0)(_*_) // same as (x,y) => x * y


    def apply[A](as: A*): List[A] = // 可変長引数
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
    
    def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
        case Nil => a2
        case Cons(h,t) => Cons(h, append(t, a2))
    }

}