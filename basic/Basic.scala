object MyModule {
    def abs(n: Int): Int =
        if (n < 0) -n
        else n

    // 末尾再帰
    def factorial(n: Int): Int = {
        // コンパイル時に末尾呼び出しの除去が成功したかをチェックするアノテーション
        @annotation.tailrec
        def go(n: Int, acc: Int): Int =
            if (n <= 0) acc
            else go(n-1, n*acc)

        go(n, 1)
    }

    // 結果をフォーマット表示する
    def formatResult(name: String, n: Int, f: Int => Int) = {
        val msg = "The %s of %d is %d"
        msg.format(name, n, f(n))
    }

    def main(args: Array[String]): Unit =
        println(formatResult("absoluto value", -42, abs))
        println(formatResult("factorial", 7, factorial))

}

object PolymorphicFunctions {

    // 配列内の要素を検索する多相関数
    def findFirst[A](as: Array[A], p: A => Boolean): Int = {
        @annotation.tailrec
        def loop(n: Int): Int =
            if (n >= as.length) -1
            else if (p(as(n))) n
            else loop(n+1)
        
        loop(0)
    }

    def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
        def go(n: Int): Boolean =
            if (n >= as.length-1) true
            else if (ordered(as(n), as(n+1))) false
            else go(n+1)
        
        go(0)
    }

    // 多相型を使う場合，全ての型に対応するために実装内容はかなりシンプルになる（制限される）
    def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
        (b: B) => f(a,b)

    // => は右結合である
    def curry[A,B,C](f: (A,B) => C): A => (B => C) =
        a => b => f(a,b)

    def uncurry[A,B,C](f: A => B => C): (A, B) => C =
        (a,b) => f(a)(b)

    def compose[A,B,C](f: B => C, g: A => B): A => C =
        a => f(g(a))

}