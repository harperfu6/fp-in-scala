// Optionはエラーの場合Noneを返すだけなので，情報を追加できるように拡張する
import scala.{Option => _, Either => _, _}

sealed trait Either[+E,+A] {
    def map[B](f: A => B): Either[E, B] = this match {
        case Right(a) => Right(f(a))
        case Left(e) => Left(e)
    }
    // 以下未実装

}

case class Left[+E](get: E) extends Either[E,Nothing] // エラー用
case class Right[+A](get: A) extends Either[Nothing,A] // 通常用

// コンパニオンオブジェクトも未実装