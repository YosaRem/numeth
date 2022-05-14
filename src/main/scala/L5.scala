object L5 extends App {
    val fp: Double => Double = x => Math.pow(x, 3) / 2
    val y0: Double = 0.5

    val xs10: List[Double] = (0 until 9).foldLeft(List(0.0)){
        case (li, _) => (li.head + 0.1) :: li
    }.reverse
    val xs20: List[Double] = (0 until 20).foldLeft(List(1.0/20)){
        case (li, _) => (li.head + 1.0/20) :: li
    }.reverse
    val xs30: List[Double] = (0 until 30).foldLeft(List(1.0/30)){
        case (li, _) => (li.head + 1.0/30) :: li
    }.reverse

    def euler(fp: Double => Double, xs: List[Double], y0: Double, lambda: Double): List[Double] = {
        def rec(acc: List[Double], xsRec: List[Double]): List[Double] = xsRec match {
            case head :: tail => rec((acc.head + lambda*fp(head)) :: acc, tail)
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
                rec((acc.head + (1.0/6)*(k1 + k4 + 2*(k2 + k3))) :: acc, tail)
            }
            case _ => acc
        }
        rec(List(y0), xs).reverse
    }

    def adams(): List[Double] = ???

    val eulerY10 = euler(fp, xs10, y0, 1.0/10)
    val eulerY20 = euler(fp, xs20, y0, 1.0/20)
    val eulerY30 = euler(fp, xs30, y0, 1.0/30)

    val rungeKutY10 = rungeKut(fp, xs10, y0, 1.0/10)
    val rungeKutY20 = rungeKut(fp, xs20, y0, 1.0/20)
    val rungeKutY30 = rungeKut(fp, xs30, y0, 1.0/30)

    println(eulerY10)
    println(rungeKutY10)

    println(eulerY20)
    println(rungeKutY20)

    println(eulerY30)
    println(rungeKutY30)

}
