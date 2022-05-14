object L5 extends App {
    val fp: Double => Double = x => 3 / (2 * Math.sqrt(x))
    val f: Double => Double = x => 3* Math.sqrt(x) - 2
    val y0: Double = 1

    def makeXs(a: Double, b: Double, N: Int): List[Double] = {
        def rec(prev: Double, lambda: Double, acc: List[Double]): List[Double]= {
            if (Math.abs(prev - lambda - a) <= 0.0000001) {
                acc
            } else {
                rec(prev - lambda, lambda, prev - lambda :: acc)
            }
        }
        rec(b, (b - a)/N, List(b)).reverse
    }

    val xs10: List[Double] = makeXs(0, 1, 10)
    val xs20: List[Double] = makeXs(0, 1, 20)
    val xs30: List[Double] = makeXs(0, 1, 30)

    def euler(fp: Double => Double, xs: List[Double], y0: Double, lambda: Double): List[Double] = {
        def rec(acc: List[Double], xsRec: List[Double]): List[Double] = xsRec match {
            case head :: tail => rec((acc.head - lambda*fp(head)) :: acc, tail)
            case _ => acc
        }
        rec(List(y0), xs).reverse
    }

    def rungeKut(fp: Double => Double, xs: List[Double], y0: Double, lambda: Double): List[Double] = {
        def rec(acc: List[Double], xsRec: List[Double]): List[Double] = xsRec match {
            case head :: tail => {
                val k1 = lambda * fp(head)
                val k2 = lambda * fp(head + lambda/2)
                val k3 = lambda * fp(head + lambda/2)
                val k4 = lambda * fp(head + lambda)
                rec((acc.head - (1.0/6)*(k1 + k4 + 2*(k2 + k3))) :: acc, tail)
            }
            case _ => acc
        }
        rec(List(y0), xs).reverse
    }

    def adams(xs: List[Double], f0: Double, f1: Double, f2: Double, lambda: Double): List[Double] = {
        def rec(acc: List[Double], xsRec: List[Double]): List[Double] = {
            if (xsRec.length == 2) {
                acc
            } else xsRec match {
                case x0 :: x1 :: x2 :: tail => {
                    val nextY = acc.head - (lambda/12)*(23*fp(x2) - 16*fp(x1) + 5*fp(x0))
                    rec(nextY :: acc, x1 :: x2 :: tail)
                }
            }
        }
        rec(f2 :: f1 :: f0 :: Nil, xs).reverse
    }

    val eulerY10 = euler(fp, xs10, y0, 1.0/10)
    val eulerY20 = euler(fp, xs20, y0, 1.0/20)
    val eulerY30 = euler(fp, xs30, y0, 1.0/30)

    val rungeKutY10 = rungeKut(fp, xs10, y0, 1.0/10)
    val rungeKutY20 = rungeKut(fp, xs20, y0, 1.0/20)
    val rungeKutY30 = rungeKut(fp, xs30, y0, 1.0/30)

    val adamsY10 = adams(xs10, rungeKutY10(0), rungeKutY10(1), rungeKutY10(2), 1.0/10)
    val adamsY20 = adams(xs20, rungeKutY20(0), rungeKutY20(1), rungeKutY20(2), 1.0/20)
    val adamsY30 = adams(xs30, rungeKutY30(0), rungeKutY30(1), rungeKutY30(2), 1.0/30)

    println(eulerY10)
    println(eulerY20)
    println(eulerY30)
    println()
    println(rungeKutY10)
    println(rungeKutY20)
    println(rungeKutY30)
    println()
    println(adamsY10)
    println(adamsY20)
    println(adamsY30)
}
