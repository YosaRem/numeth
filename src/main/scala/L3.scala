import scala.collection.View.FlatMap

object L3 extends App {

    case class Matrix3(data: Seq[Seq[Double]]) {
        override def toString: String =
            data.map(x => x.mkString("  ")).mkString("\n")
    }

    implicit def to(value: ((Double, Double), Double)): List[Double] = {
        val v = (value._1._1, value._1._2, value._2)
        v._1 :: v._2 :: v._3 :: Nil
    }

    case class Methods(input: Matrix3) {
        def gauss(): Seq[Double] = {
            def toStair(i: Int, data: Seq[Seq[Double]]): Matrix3 = {
                val toOneOnIndex: (Int, Seq[Seq[Double]]) => Matrix3 = (idx, data) => {
                    val res = for {
                        j <- 0 until 3
                        line = if (j == idx) data(j).map(_ * 1d / data(idx)(idx)) else data(j)
                    } yield line
                    Matrix3(res)
                }
                if (i == 3) {
                    Matrix3(data)
                } else {
                    val withOne = toOneOnIndex(i, data)
                    val resLines = for {
                        j <- 0 until 3
                        toPlus = withOne.data(j)(i)
                        t = if (j <= i) withOne.data(j) else withOne.data(j).zipWithIndex.map(x => x._1 - toPlus * withOne.data(i)(x._2))
                    } yield t
                    toStair(i + 1, resLines)
                }
            }

            toStair(0, input.data)
                .data.foldRight(List.empty[Double])((elem, acc) => {
                if (acc.isEmpty) {
                    elem.last :: Nil
                } else {
                    (elem.last - elem.init.reverse.zip(acc.reverse).map(x => x._1 * x._2).sum) :: acc
                }
            })
        }

        def centerGauss(): Seq[Double] = {
          Methods(order()).gauss()
        }

        def jacobi(): Seq[Double] = {
            val roots = for {
                i <- 0 until 3
                r = input.data(i).last / input.data(i)(i)
            } yield r

            def jacobiRec(ma: Matrix3, roots: Seq[Double], iterCount: Int): Seq[Double] = {
                val newRoots = for {
                    i <- 0 until 3
                    zipRoots = ma.data(i).init.zip(roots).map(x => x._1 * x._2)
                    n = (ma.data(i).last - zipRoots.take(i).sum - zipRoots.drop(i + 1).sum) / ma.data(i)(i)

                } yield n
                if (norm(roots, newRoots) < 0.5 * 0.0001) {
                    println("Количество интерация метода Якоби: " + iterCount.toString)
                    newRoots
                } else {
                    jacobiRec(ma, newRoots, iterCount + 1)
                }
            }
            jacobiRec(order(), roots, 0)
        }

        def gaussSeidel(): Seq[Double] = {
            val roots = for {
                i <- 0 until 3
                r = input.data(i).last / input.data(i)(i)
            } yield r

            def rec(ma: Matrix3, roots: Seq[Double], iterCount: Int): Seq[Double] = {
                val r: collection.mutable.Seq[Double] = collection.mutable.Seq(roots: _*)
                val newRoots = for {
                    i <- 0 until 3
                    zipRoots = ma.data(i).init.zip(r).map(x => x._1 * x._2)
                    n = (ma.data(i).last - zipRoots.take(i).sum - zipRoots.drop(i + 1).sum) / ma.data(i)(i)
                    _ = r(i) = n
                } yield n
                if (norm(roots, newRoots) < 0.5 * 0.0001) {
                    println("Количество интерация метода Гаусса-Зейделя: " + iterCount.toString)
                    newRoots
                } else {
                    rec(ma, newRoots, iterCount + 1)
                }
            }
            rec(order(), roots, 0)
        }

        private def norm(x: Seq[Double], y: Seq[Double]): Double = {
            math.sqrt(x.zip(y).map(i => math.pow(i._1 - i._2, 2)).sum)
        }

        private def order(): Matrix3 = {
            Matrix3(
                for {
                    i <- 0 until 3
                    line = input.data.maxBy(x => math.abs(x(i)))
                } yield line
            )
        }
    }


    val input = Matrix3(
        List(
            List(-0.03, 0.2, 1.67, 2.21),
            List(0.83, 0.34, -0.1, 2.58),
            List(-0.37, -0.5, -0.13, -2.37)
        )
    )
    println("Метод Гаусса:")
    println(Methods(input).gauss())
    println()

    println("Метод Гаусса с выбором главного элемента: ")
    println(Methods(input).centerGauss())
    println()

    println("Метод Якоби:")
    println(Methods(input).jacobi())

    println("Метод Гаусса-Зейделя")
    println(Methods(input).gaussSeidel())
}
