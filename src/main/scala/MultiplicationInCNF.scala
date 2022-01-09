import AdditionInCNF.returnCnfAndZ
import Util._

import scala.sys.process.Process

object MultiplicationInCNF {

  val X = "XX"
  val Y = "YY"

  /* binaryString provides bool variables */
  def provideByBinary(binaryString: String,
                      variables: Seq[Bool]): Seq[Formula] = {
    binaryString.reverseIterator
      .zip(variables.toIterator)
      .map {
        case (c, bool) =>
          c match {
            case '1' => bool
            case '0' => !bool
            case _   => throw new UnsupportedOperationException
          }
      }
      .toSeq
  }

  def shiftOneBit(binaryString: String): String = binaryString.head match {
    case '1' => throw new Exception("insufficient bit width.")
    case '0' => binaryString.tail + "0"
    case _   => throw new UnsupportedOperationException
  }

  def returnCnfOutputAndAnswerBool(num1: Int,
                                   num2: Int): (CnfOutput, Seq[Bool]) = {
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

    /*
     * cnf: 最後にANDで繋がれるClauseの配列
     *   w: 前加算の結果によって規定されるBool変数(1bit1変数)
     *  _x: 加算される値（2進数表記）
     * */
    case class ForCalc(cnf: Seq[Formula], w: Seq[Bool], _x: String)
    // 前加算。最初は0なので0を規定する。
    val initial = (0 until maxLength).map(_ => Bool())
    val zero = "0" * maxLength
    ans = ans ++ provideByBinary(zero, initial)
    val init = ForCalc(ans, initial, b1)

    // b2ではなく_b2を使う
    val result = _b2.reverseIterator.zipWithIndex.foldLeft(init) {
      case (acc, (yChar, i)) =>
        val prefix = ((i + 1) * 100).toString
        val ex = acc._x
        print(i)
        if (yChar == '1') {
          // exに対応するBoolを生成
          val exBool = ex.map(_ => Bool(Some(prefix)))
          // そのBoolはexによって規定される
          val tmp = acc.cnf ++ provideByBinary(ex, exBool)
          // 「"前の加算の結果として使用されているBool"と"exのBool"の加算」を規定する
          val (clauses, nextW) = returnCnfAndZ(acc.w, exBool, Some(prefix))
          // 加算が規定するclauseを追加。また結果となっている変数は次で使用する
          ForCalc(tmp ++ clauses, nextW, shiftOneBit(ex))
        } else {
          // _xを1bitシフトするだけであとは何もしない。
          ForCalc(acc.cnf, acc.w, shiftOneBit(ex))
        }
    }

    // result.wが答えなので、あとから数字に直すために使う。
    (updateOutput(result.cnf, CnfOutput("")), result.w)
  }

  def returnResult(boolInInterest: Seq[Bool]): Int = {
    val reader = getFileReader("miniSAT.txt")
    val header = reader.readLine()
    header match {
      case "UNSAT" => println("fail"); -1
      case "SAT" => {
        val interestNum = boolInInterest.map(_.name)
        val answer =
          reader
            .readLine()
            .split(" ")
            .filter { str =>
              val tmp = if (str.startsWith("-")) str.tail else str
              interestNum.contains(tmp)
            }
        answer
          .foldLeft(0, 1) {
            case (acc, d) =>
              (acc._1 + (if (d.startsWith("-")) 0 else acc._2), acc._2 * 2)
          }
          ._1
      }
    }
  }

  def exec(num1: Int, num2: Int): Int = {
    val cnfOutputAndAnswerBool = returnCnfOutputAndAnswerBool(num1, num2)
    cnfOutputAndAnswerBool._1.addHeader().makeMiniSatSolve()
    returnResult(cnfOutputAndAnswerBool._2)
  }
}
