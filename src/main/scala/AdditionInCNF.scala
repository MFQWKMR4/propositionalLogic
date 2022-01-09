import MultiplicationInCNF.{provideByBinary, returnResult}
import Util.updateOutput

object AdditionInCNF {

  val X = "X"
  val Y = "Y"
  val Z = "Z"
  val C = "C"

  def returnCnfAndZ(
    num1: Seq[Bool],
    num2: Seq[Bool],
    boolPrefix: Option[String] = None
  ): (Seq[Formula], Seq[Bool]) = {

    val maxLength = num1.length max num2.length
    // prepare variables
    Bool.resetCount()
    val variableMap =
      Map(
        X -> num1,
        Y -> num2,
        Z -> (0 until maxLength).map(_ => Bool(boolPrefix)),
        C -> (Bool("dummy") +: (1 until maxLength).map(_ => Bool(boolPrefix)))
      )

    var ans = Seq[Formula]()

    // (z0 or not xor(x0,y0)) and (not z0 or xor(x0,y0))
    val x0 = variableMap(X)(0)
    val y0 = variableMap(Y)(0)
    val z0 = variableMap(Z)(0)
    val provideZ0 = Seq(
      z0 || (!x0 || y0),
      z0 || (x0 || !y0),
      !z0 || (x0 || y0),
      !z0 || (!x0 || !y0)
    )
    ans = ans ++ provideZ0

    // not_xor(c1, (x0 and y0))
    val c1 = variableMap(C)(1)
    val provideC0 = Seq(!c1 || x0, !c1 || y0, !x0 || !y0 || c1)
    ans = ans ++ provideC0

    for (i <- 2 until maxLength) {
      val xi_1 = variableMap(X)(i - 1)
      val yi_1 = variableMap(Y)(i - 1)
      val ci_1 = variableMap(C)(i - 1)
      val ci = variableMap(C)(i)
      // not_xor(ci, at_least_two(xi_1, yi_1, ci_1)
      val provideCi = Seq(
        !ci || xi_1 || yi_1,
        !ci || xi_1 || ci_1,
        !ci || yi_1 || ci_1,
        ci || !xi_1 || !yi_1,
        ci || !xi_1 || !ci_1,
        ci || !yi_1 || !ci_1
      )
      ans = ans ++ provideCi
    }

    for (i <- 1 until maxLength) {
      val xi = variableMap(X)(i)
      val yi = variableMap(Y)(i)
      val ci = variableMap(C)(i)
      val zi = variableMap(Z)(i)
      // not_xor(zi, xor(xor(xi_1, yi_1), ci_1))
      val provideZi = Seq(
        zi || !xi || !yi || !ci,
        zi || !xi || yi || ci,
        zi || xi || !yi || ci,
        zi || xi || yi || !ci,
        !zi || xi || yi || ci,
        !zi || xi || !yi || !ci,
        !zi || !xi || yi || !ci,
        !zi || !xi || !yi || ci,
      )
      ans = ans ++ provideZi
    }

    // not_xor(z,c)
    val ciLast = variableMap(C)(maxLength - 1)
    val ziLast = variableMap(Z)(maxLength - 1)
    val provideZLast = Seq(ciLast || !ziLast, !ciLast || ziLast)
    ans = ans ++ provideZLast

    (ans, variableMap(Z))
  }

  def exec(num1: Int, num2: Int): Int = {
    val _b1 = num1.toBinaryString
    val _b2 = num2.toBinaryString

    val maxLength = _b1.length + _b2.length

    // prepare binary string
    val b1 = s"%${maxLength}s".format(_b1).replaceAll(" ", "0")
    val b2 = s"%${maxLength}s".format(_b2).replaceAll(" ", "0")

    // Clauseの配列
    var ans = Seq[Formula]()

    // prepare variables
    Bool.resetCount()
    val variableMap =
      Map(
        X -> (0 until maxLength).map(_ => Bool()),
        Y -> (0 until maxLength).map(_ => Bool())
      )
    ans = ans ++ provideByBinary(b1, variableMap(X))
    ans = ans ++ provideByBinary(b2, variableMap(Y))

    val cnfAndAns = returnCnfAndZ(variableMap(X), variableMap(Y), Some("10"))
    val cnfOutPut = updateOutput(ans ++ cnfAndAns._1, CnfOutput(""))
    cnfOutPut.addHeader().makeMiniSatSolve()
    returnResult(cnfAndAns._2)
  }
}
