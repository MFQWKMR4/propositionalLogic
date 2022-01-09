import Util.{printMiniSatResult, toCnfOutput}

import scala.language.postfixOps

object Report1 {
  def run(): Unit = {
    val a = 93
    val b = 35
    val res = AdditionInCNF2.exec(a, b)
    assert(a + b == res)
    println(res)
  }
}

object Report2 {
  def run(): Unit = {
    val a = 15
    val b = 4
    val res = MultiplicationInCNF.exec(a, b)
    assert(a * b == res)
    println(res)
  }
}

object Report4 {

  /**
    * Implement Tseitin conversion.
    */
  def run(): Unit = {
    Bool.resetCount()
    val x1 = Bool()
    val x2 = Bool()
    val x3 = Bool()
    val x4 = Bool()
    val x5 = Bool()

    val case1 = (x1 && x2 && !x3 && x4 && !x5) || (x1 && x2 && !x3 && !x4 && x5) || (!x1 && x2 && x3 && !x4 && x5)
    val case2 = !x1 && case1
    val case3 = case2 || (x1 && x2 && !x3 && x4 && !x5)

    Seq(case1, case2, case3).foreach { f =>
      /* Tseitin conversion */
      val converted = TseitinConversion.exec(f)

      // 変換後のclauseの集合を表示
      println(
        converted
          .map(_.literals.map(_.toDisplay).mkString(" ∨ "))
          .mkString(" ),( ")
      )

      // SATソルバで解く
      toCnfOutput(converted).makeMiniSatSolve()
      printMiniSatResult()
    }
  }
}

object Report5 {

  def run() = {

    val nl = "\n"
    val div = "	"

    /**
      *
      * https://sudoku.ara.black/problem/3/index.htm
      * こちらの問題をそのままコピペして入力とすることができる
      *
      * */
    val input1 = """
                  |　	９	７	　	　	　	　	　	　
                  |　	　	　	　	６	　	７	　	３
                  |　	３	　	　	　	　	　	　	５
                  |　	　	　	８	　	７	　	　	　
                  |　	８	　	　	　	　	　	９	　
                  |　	　	　	６	　	５	　	　	　
                  |４	　	　	　	　	　	　	７	　
                  |６	　	５	　	１	　	　	　	　
                  |　	　	　	　	　	　	２	８	　""".stripMargin

    Sudoku.solve(input1.split(nl).flatMap(_.split(div))).makeMiniSatSolve()
    Sudoku.display()

    println("###########################################################")

    val input2 = """
        |　	７	　	３	　	　	　	　	　
        |　	　	２	５	　	　	　	　	８
        |　	　	１	　	　	　	９	４	　
        |　	　	　	　	　	　	　	２	７
        |　	　	　	　	　	　	　	　	　
        |５	４	　	　	　	　	　	　	　
        |　	５	７	　	　	　	３	　	　
        |９	　	　	　	　	１	２	　	　
        |　	　	　	　	　	４	　	９	　""".stripMargin
    Sudoku.solve(input2.split(nl).flatMap(_.split(div))).makeMiniSatSolve()
    Sudoku.display()

    println("###########################################################")

    val input3 = """
        |　	　	１	５	　	　	　	３	　
        |　	　	　	１	　	　	　	７	２
        |４	　	　	　	　	　	　	　	　
        |８	９	　	　	　	６	　	　	　
        |　	　	　	　	　	　	　	　	　
        |　	　	　	３	　	　	　	５	７
        |　	　	　	　	　	　	　	　	１
        |６	２	　	　	　	８	　	　	　
        |　	８	　	　	　	９	４	　	　""".stripMargin

    Sudoku.solve(input3.split(nl).flatMap(_.split(div))).makeMiniSatSolve()
    Sudoku.display()

    println("###########################################################")
  }
}

object Report6 {
  def run() = {
    //  http://www.jaist.ac.jp/~mizuhito/Lecture/Fix-a-Pix/problem1_80x50.txt
    FixAPix.solve_problem1_8050().makeMiniSatSolve()
    FixAPix.display_problem1_8050()

    println("end")
  }
}
object Report7 {}
object Report8 {}

object Report9 {

  import scala.language.implicitConversions
  implicit def symbol2bool(p: Symbol): Bool = Bool(p.name)

  def run(): Unit = {

    val case1 = Sequent(List('p && ('q || 'r)), List(('p || 'q) && ('p || 'r)))

    val case2 = Sequent(List(), List(('a ==> 'b) || ('b ==> 'a)))

    val case3 = Sequent(List('p && ('q || 'r)), List('p && 'r))

    Seq(case1, case2, case3).foreach { seq =>
      LK.prove(seq) match {
        case Some(prf) => prf.getProofFigure()
        case None      => println("unprovable")
      }
    }
  }
}

object main extends App {
  Report4.run()
}
