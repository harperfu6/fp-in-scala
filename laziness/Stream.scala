import Stream._ 

// Stream.fn()で呼び出すもの
trait Stream[+A] {

    def toList: List[A] = {
        @annotation.tailrec
        def go(s: Stream[A], acc: List[A]): List[A] = s match {
            case Cons(h,t) => go(t(), h() :: acc)
            case _ => acc
        }
        go(this, List()).reverse
    }

    // 遅延評価版foldRight
    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
        // 引数fの第２引数は，名前渡しで遅延評価の選択が可能であることを示す
        case Cons(h,t) => f(h(), t().foldRight(z)(f))
        case _ => z
    }

    def exists(p: A => Boolean): Boolean =
        // 遅延評価版foldRightを使うことによって
        // p(a)がtrueの場合は，残りのbを評価しない
        foldRight(false)((a,b) => p(a) || b)

    def headOption: Option[A] = this match {
        case Empty => None
        case Cons(h,t) => Some(h()) // h()でhサンクを強制的に評価，評価後はキャッシュされた（遅延値）が返される
    }

    // 無限ストリームの展開用
    def take(n: Int): Stream[A] = this match {
        case Cons(h,t) if n > 1 => cons(h(), t().take(n-1))
        case Cons(h,_) if n == 1 => cons(h(), empty)
        case _ => empty
    }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

// fn()で呼び出すもの
object Stream {
    // 引数をサンクに入れる（遅延評価）
    // () => A のシンタックスシュガー
    // 遅延評価はいわば，引数空でA型を返す関数を渡すということ
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
        // lazyに入れることによって最初に参照するまで評価しない
        // 加えて，繰り返し参照されないように結果はキャッシュされる
        lazy val head = hd 
        lazy val tail = tl
        Cons(() => head, () => tail)
    }
    def empty[A]: Stream[A] = Empty

    // Stream.apply(1, 2, 3) -> Stream(1, 2, 3)と表すため
    // 可変長型アノテーションは，Seq[A]のシンタックスシュガーにすぎない
    def apply[A](as: A*): Stream[A] =
        if (as.isEmpty) empty
        else cons(as.head, apply(as.tail: _*)) //_*型アノテーションを使えば引数が可変長のメソッドに渡せる
    
    // 無限ストリーム
    // しかしサンクを評価するまでは展開されない
    val ones: Stream[Int] = Stream.cons(1, ones)
}
