object L4 extends App {

    def trapezoid(f: Double => Double, a: Double, b: Double, h: Double): Unit = {
        def rec(acc: Double, count: Int, xNow: Double): Unit = {
            if (count == 0) {
                println(s"Шаг: $h; Значение: ${acc * h/2}")
            } else {
                rec(acc + (f(xNow) + f(xNow + h)) , count - 1, xNow + h)
            }
        }
        rec(0, Math.round((b - a)/h).toInt, a)
    }

    def three_8(f: Double => Double, a: Double, b: Double, h: Double): Unit = {
        val xsFor3 = for {
            s <- 1 until Math.round((b - a)/h).toInt
        } yield if (s % 3 == 0) 0 else (a + s*h)

        val xsFor2 = for {
            s <- 3 to Math.round((b - a)/h).toInt - 3
        } yield if (s % 3 == 0) (a + s*h) else 0


        println((f(a) + f(b) + 2*xsFor2.map(f).sum + 3*xsFor3.map(f).sum)*h*3/8.0)
    }

    val f: Double => Double = x => Math.log(1 + Math.sin(x))

    println("Трапеции")
    trapezoid(f, 2, 3, 0.1)
    trapezoid(f, 2, 3, 0.05)
    trapezoid(f, 2, 3, 0.025)

    println("\n3/8")
    three_8(f, 2, 3, 0.1)
    three_8(f, 2, 3, 0.05)
    three_8(f, 2, 3, 0.025)
}
