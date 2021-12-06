import math.E

object App extends App {
  case class Section(start: Double, end: Double) {
    require(start < end)
  }
  case class Input(f: Double => Double,
                   fp: Double => Double,
                   fpp: Double => Double,
                   section: Section,
                   epsilon: Double)

  val iterationCount = scala.collection.mutable.Map[String, Int]()

  case class Methods(input: Input) {
    private def selectFromSection(section: Section): Double = {
      if (input.fpp(section.start) * input.f(section.start) > 0)
        section.start
      else
        section.end
    }

    private def half: Section => Double = x => (x.end + x.start) / 2


    private def Ep = input.epsilon

    def halfDivide: this.type = {
      val count = math.ceil(math.log((input.section.end - input.section.start) / Ep) / math.log(2)).toInt
      def halfRec(c: Int, section: Section): Unit = {
        if (c == 0) {
          iterationCount.addOne("Половинное деление" -> count)
          println(s"Метод половинного деления: \nОтвет между [${section.start}, ${section.end}] \n${half(section)}\n")
        }
        else {
          val sectionHalf = half(section)
          val start = input.f(section.start)
          val middle = input.f(sectionHalf)
          if (start * middle < 0)
            halfRec(c - 1, Section(section.start, sectionHalf))
          else
            halfRec(c - 1, Section(sectionHalf, section.end))
        }
      }
      halfRec(count, input.section)
      this
    }

    def newton: this.type = {
      def newtonRec(c: Int, prevX: Double, nextX: Double): Unit = {
        if (math.abs(input.f(nextX) - input.f(prevX)) < Ep) {
          iterationCount.addOne("Ньютон" -> c)
          println(s"Метод Ньютона: $nextX\n")
        } else {
          val next = nextX - (input.f(nextX) / input.fp(nextX))
          newtonRec(c + 1, nextX, next)
        }
      }
      newtonRec(0, 0, selectFromSection(input.section))
      this
    }

    def upgradeNewton: this.type = {
      val divider = input.fp(selectFromSection(input.section))
      def newtonRec(c: Int, prevX: Double, nextX: Double): Unit = {
        if (math.abs(input.f(nextX) - input.f(prevX)) < Ep) {
          iterationCount.addOne("Модифицированный Ньютон" -> c)
          println(s"Модифицированный метод Ньютона: $nextX\n")
        } else {
          val next = nextX - (input.f(nextX) / divider)
          newtonRec(c + 1, nextX, next)
        }
      }
      newtonRec(0, 0, selectFromSection(input.section))
      this
    }

    def chord: this.type = {
      val bound = selectFromSection(input.section)
      def chordRec(c: Int, prevX: Double, nextX: Double): Unit = {
        if (math.abs(input.f(nextX) - input.f(prevX)) < Ep) {
          iterationCount.addOne("Хорды" -> c)
          println(s"Метод Хорд: $nextX\n")
        } else {
          val next = nextX - ((input.f(nextX) / (input.f(nextX) - input.f(bound))) * (nextX - bound))
          chordRec(c + 1, nextX, next)
        }
      }
      if (bound == input.section.start) {
        chordRec(0, 0, input.section.end)
      } else
        chordRec(0, 0, input.section.start)
      this
    }

    def moveChord: this.type = {
      def chordRec(c: Int, prevX: Double, nextX: Double): Unit = {
        if (math.abs(input.f(nextX) - input.f(prevX)) < Ep) {
          iterationCount.addOne("Движущиеся хорды" -> c)
          println(s"Метод подвижных хорд: $nextX\n")
        } else {
          val next = nextX - ((input.f(nextX) / (input.f(nextX) - input.f(prevX))) * (nextX - prevX))
          chordRec(c + 1, nextX, next)
        }
      }
      val bound = selectFromSection(input.section)
      if (bound == input.section.start) {
        chordRec(0, bound, input.section.end)
      } else
        chordRec(0, bound, input.section.start)
      this
    }

    def simpleIteration(phi: Double => Double): Unit = {
      def iterRec(c: Int, prevX: Double): Unit = {
        val next = phi(prevX)
        if (math.abs(next - prevX) < Ep) {
          iterationCount.addOne("Простые итерации" -> c)
          println(s"Метод простых итераций: $next\n")
        } else {
          iterRec(c + 1, next)
        }
      }
      iterRec(0, inputData.section.end)
    }
  }

  val inputData = Input(
    f = x => math.pow(E, -x) - 2.7 + math.pow(x, 2),
    fp = x => 2 * x - math.pow(E, -x),
    fpp = x => 2 + math.pow(E, -x),
    section = Section(1.2, 1.8),
    epsilon = 0.5 * 10e-6
  )

  Methods(inputData)
      .halfDivide
      .newton
      .upgradeNewton
      .chord
      .moveChord
      .simpleIteration(x => x - inputData.f(x)/inputData.fp(x))
  println("Количество итераций:")
  println(
    iterationCount
      .toList
      .sortBy(_._2)
      .map {
        case (key, value) => s"$key: $value"
      }
      .mkString("\n")
  )
}
