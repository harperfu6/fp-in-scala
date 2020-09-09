// hide std library
import scala.{Option => _, Either => _, _}

// コンパニオンオブジェクトではなく，トレイト本体で定義する
// fn(obj, arg1)ではなく
// obj.fn(arg1) or obj fn arg1 で呼び出せるようにする
sealed trait Option[+A] {
    def map[B](f A => B): Option[B] = this match {
        case None => None
        case Some(a) => Some(f(a))
    }
    // BはAの型かスーパークラス([B >: A])
    // AをOptionの共変パラメータとして宣言しても問題ないことをScalaに納得させる
    // 遅延評価アノテーション(default: => B)
    def getOrElse[B >: A](default: => B): B  = this match {
        case None => default
        case Some(a) => a
    }

    def flatMap[B](f: A => Option[B]): Option[B] = // 失敗する可能性のあるfを適用
        map(f) getOrElse None

    def orElse[B >: A](ob: => Option[B]): Option[B] =
        this map (Some(_)) getOrElse ob

    def filter(f: A => Boolean): Option][A] = this match {
        case Some(a) if f(a) => this
        case _ => None
    }

}
case object None extends Option[Nothing]
case class Some[+A](get: A) extends Option[A]

object Option {

    def mean(xs: Seq[Double]): Option[Double] =
        if (xs.isEmpty) None
        else Some(xs.sum / xs.length)
    
    def variane(xs: Seq[Double]): Option[Double] =
        mean(xs) flatMap (m => mean(xs.map(x => math.pow(x-m,2))))
    
    // リストをmapし，さらにリスト全体へ展開する関数を一般化する
    // 例えば，List[String] -> Option[List[Int]]にする場合
    // mapでList[Option[Int]]としてさらにOptionを外に出すのはリストを２回走査することになって非効率である
    def map2[A,B,C][a: Option[A], b: Option[B]](f: (A,B) => C): Option[C] =
        a flatMap (aa => b map (bb => f(aa, bb)))

    def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
        case Nil => Some(Nil)
        case h::t => map2(f(h), traverse(t)(f))(_ :: _)
    }
