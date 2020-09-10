trait RNG {
    def nextInt: (Int, RNG) // 状態を副作用として更新するのではなく返却する
}


object RNG {

    // 線形合同法と呼ばれるもの
    case class Simple(seed: Long) extends RNG {
        def nextInt: (Int, RNG) = {
            val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // &はビット論理積
            val nextRNG = SimpleRNG(newSeed)
            val n = (newSeed >>> 16).toInt // >>>は0埋め右バイナリシフト
            (n, nextRNG)
        }
    }

    // RNGを遷移させる状態遷移データ型
    // Rand[+A]: ランダムに生成されたA
    type Rand[+A] = RNG => (A, RNG)

    val int: Rand[Int] = _.nextInt

    def nonNegativeInt(rng: RNG): (Int, RNG) = {
        val (i, r) = rng.nextInt
        (if (i < 0) -(i+1) else i, r)
    }

    def double(rng: RNG): (Double, RNG) = {
        val (i, r) = nonNegativeInt(rng)
        (i / (Int.MaxValue.toDouble + 1), r)
    }

    // RNGの状態を明示的にやり取りするのを回避した上でRandアクションを結合するためのコンビネータ
    // 受け渡しの全てを自動的に行うDSLのようなもの

    // RNGの単純な状態遷移であり，RNGの状態を未使用のまま渡し常に定数値を返す
    // 要はRand[+A]を返すだけである
    def unit[A](a: A): Rand[A] =
        rng => (a, rng)

    // 状態そのものを変化させずに状態アクションを出力する
    def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
        rng => {
            val (a, rng2) = s(rng)
            (f(a), rng2)
        }
    
    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C]
        rng => {
            val (a, r1) = ra(rng)
            val (b, r2) = rb(r1)
            (f(a,b), r2)
        }
    // A型の値を生成するアクションとB型の値を生成するアクションを組み合わせてAとBのペアを生成するアクション
    // 任意のRNG状態のアクションの結合に利用可能
    def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
        map2(ra, rb)((_, _))
    
    val randIntDouble: Rand[(Int, Double)] =
        both(int, double)
    
    val randDoubleInt: Rand[(Double, Int)] =
        both(double, int)

}