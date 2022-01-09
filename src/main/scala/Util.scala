import java.io.BufferedReader
import scala.sys.process.Process

object Util {

  def flattenAnd(f: Formula): Set[Formula] = f match {
    case And(f1, f2) => flattenAnd(f1) | flattenAnd(f2)
    case _           => Set(f)
  }

  def flattenOr(f: Formula): Set[Formula] = f match {
    case Or(f1, f2) => flattenOr(f1) | flattenOr(f2)
    case _          => Set(f)
  }

  def cnfToClauseSet(cnf: Formula): Set[Clause] = {
    flattenAnd(cnf).map { g =>
      Clause(flattenOr(g).map {
        case p: Bool      => Literal(p, false)
        case Neg(p: Bool) => Literal(p, true)
        case _            => throw new IllegalArgumentException
      })
    }
  }

  def toClause(flattenedOr: Set[Formula]): Clause = {
    Clause(flattenedOr.map {
      case bool: Bool      => Literal(bool, false)
      case Neg(bool: Bool) => Literal(bool, true)
    })
  }

  def toOutputInCNF(f: Formula): String = {
    f match {
      case or: Or     => toOutputInCNF(or.f1) + " " + toOutputInCNF(or.f2)
      case bool: Bool => bool.name
      case neg: Neg =>
        neg.f1 match {
          case bool: Bool => "-" + bool.name
          case e          => throw new UnsupportedOperationException(e.expression)
        }
      case e => throw new UnsupportedOperationException(e.expression)
    }
  }

  def updateOutput(formulasInCNF: Seq[Formula],
                   output: CnfOutput): CnfOutput = {
    formulasInCNF.foldLeft(output) {
      case (acc, f) => acc.update(toOutputInCNF(f))
    }
  }

  def toCnfOutPutRow(clause: Clause): String = {
    clause.literals.map(_.toDisplay).mkString(" ")
  }

  def toCnfOutput(cnf: Set[_ <: Clause]): CnfOutput = {
    var ans = ""

    println("cnf.size")
    println(cnf.size)
    cnf
      .map(toCnfOutPutRow)
      .foreach { a =>
        ans += a + " 0\n"
      }

    CnfOutput(ans).addHeader()
  }

  def toAndFormula(seq: Seq[Formula]): Formula = {
    val init = seq.take(2)
    val rest = seq.drop(2)

    rest.foldLeft(And(init.head, init.tail.head)) {
      case (acc, e) => And(acc, e)
    }
  }

  def toOrFormula(seq: Seq[Formula]): Formula = {
    val init = seq.take(2)
    val rest = seq.drop(2)

    rest.foldLeft(Or(init.head, init.tail.head)) {
      case (acc, e) => Or(acc, e)
    }
  }

  def getFileReader(filename: String): BufferedReader = {
    import java.io.{BufferedReader, FileInputStream, InputStreamReader}
    import scala.util.DynamicVariable
    new DynamicVariable[BufferedReader](
      new BufferedReader(
        new InputStreamReader(new FileInputStream("./tmp/" + filename))
      )
    ).value
  }

  def printMiniSatResult() = {
    val reader = getFileReader("miniSAT.txt")
    println(reader.readLine())
    println(reader.readLine())
  }
}

case class CnfOutput(output: String) {
  def update(line: String): CnfOutput = {
    CnfOutput(output + line + " 0\n")
  }
  def addHeader(): CnfOutput = {
    val numVars =
      output
        .replaceAll("-", "")
        .replaceAll(" 0\n", " ")
        .split(" ")
        .distinct
        .filterNot(_ == "")
        .map(_.toInt)
        .max
    val numClauses = output.count(_ == '\n')
    val header = s"p cnf $numVars $numClauses \n"
    CnfOutput(header + output)
  }
  def print(): Unit = println(output)
  def writeOut(): Unit = {
    import java.io.PrintWriter
    val file = new PrintWriter("./tmp/result.txt")
    file.write(output)
    file.close()
  }
  def makeMiniSatSolve() = {
    this.writeOut()
    val result = Process("minisat ./tmp/result.txt ./tmp/miniSAT.txt").!
    println("exit code: " + result)
  }
}
